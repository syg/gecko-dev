/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "frontend/SourceNotesEmitHandler.h"

#include "frontend/FullEmitHandler.h"

using namespace js;
using namespace js::frontend;

static bool
AllocSrcNote(ExclusiveContext* cx, SrcNotesVector& notes, unsigned* index)
{
    // Start it off moderately large to avoid repeated resizings early on.
    // ~99% of cases fit within 256 bytes.
    if (notes.capacity() == 0 && !notes.reserve(256))
        return false;

    if (!notes.growBy(1)) {
        ReportOutOfMemory(cx);
        return false;
    }

    *index = notes.length() - 1;
    return true;
}

template <typename EmitProvider>
bool
SourceNotesEmitterImpl<EmitProvider>::newSrcNote(SrcNoteType type, unsigned* indexp)
{
    SrcNotesVector& notes = this->notes();
    unsigned index;
    if (!AllocSrcNote(cx, notes, &index))
        return false;

    /*
     * Compute delta from the last annotated bytecode's offset.  If it's too
     * big to fit in sn, allocate one or more xdelta notes and reset sn.
     */
    ptrdiff_t offset = this->offset();
    ptrdiff_t delta = offset - lastNoteOffset();
    current->lastNoteOffset = offset;
    if (delta >= SN_DELTA_LIMIT) {
        do {
            ptrdiff_t xdelta = Min(delta, SN_XDELTA_MASK);
            SN_MAKE_XDELTA(&notes[index], xdelta);
            delta -= xdelta;
            if (!AllocSrcNote(cx, notes, &index))
                return false;
        } while (delta >= SN_DELTA_LIMIT);
    }

    /*
     * Initialize type and delta, then allocate the minimum number of notes
     * needed for type's arity.  Usually, we won't need more, but if an offset
     * does take two bytes, setSrcNoteOffset will grow notes.
     */
    SN_MAKE_NOTE(&notes[index], type, delta);
    for (int n = (int)js_SrcNoteSpec[type].arity; n > 0; n--) {
        if (!newSrcNote(SRC_NULL))
            return false;
    }

    if (indexp)
        *indexp = index;
    return true;
}

template <typename EmitProvider>
bool
SourceNotesEmitHandlerImpl<EmitProvider>::newSrcNote2(SrcNoteType type, ptrdiff_t offset,
                                                      StmtInfoBCE* topStmt, unsigned* indexp)
{
    unsigned index;
    if (!newSrcNote(type, &index))
        return false;
    if (!setSrcNoteOffset(index, 0, offset, topStmt))
        return false;
    if (indexp)
        *indexp = index;
    return true;
}

template <typename EmitProvider>
bool
SourceNotesEmitHandlerImpl<EmitProvider>::newSrcNote3(SrcNoteType type, ptrdiff_t offset1,
                                                      ptrdiff_t offset2, StmtInfoBCE* topStmt,
                                                      unsigned* indexp)
{
    unsigned index;
    if (!newSrcNote(type, &index))
        return false;
    if (!setSrcNoteOffset(index, 0, offset1, topStmt))
        return false;
    if (!setSrcNoteOffset(index, 1, offset2, topStmt))
        return false;
    if (indexp)
        *indexp = index;
    return true;
}

static void
ReportStatementTooLarge(TokenStream& ts, StmtInfoBCE* topStmt)
{
    ts.reportError(JSMSG_NEED_DIET, StatementName(topStmt));
}

template <typename EmitProvider>
bool
SourceNotesEmitHandlerImpl<EmitProvider>::setSrcNoteOffset(unsigned index, unsigned which,
                                                           ptrdiff_t offset, StmtInfoBCE* topStmt)
{
    if (!SN_REPRESENTABLE_OFFSET(offset)) {
        ReportStatementTooLarge(parser->tokenStream, topStmt);
        return false;
    }

    SrcNotesVector& notes = this->notes();

    /* Find the offset numbered which (i.e., skip exactly which offsets). */
    jssrcnote* sn = &notes[index];
    MOZ_ASSERT(SN_TYPE(sn) != SRC_XDELTA);
    MOZ_ASSERT((int) which < js_SrcNoteSpec[SN_TYPE(sn)].arity);
    for (sn++; which; sn++, which--) {
        if (*sn & SN_4BYTE_OFFSET_FLAG)
            sn += 3;
    }

    /*
     * See if the new offset requires four bytes either by being too big or if
     * the offset has already been inflated (in which case, we need to stay big
     * to not break the srcnote encoding if this isn't the last srcnote).
     */
    if (offset > (ptrdiff_t)SN_4BYTE_OFFSET_MASK || (*sn & SN_4BYTE_OFFSET_FLAG)) {
        /* Maybe this offset was already set to a four-byte value. */
        if (!(*sn & SN_4BYTE_OFFSET_FLAG)) {
            /* Insert three dummy bytes that will be overwritten shortly. */
            jssrcnote dummy = 0;
            if (!(sn = notes.insert(sn, dummy)) ||
                !(sn = notes.insert(sn, dummy)) ||
                !(sn = notes.insert(sn, dummy)))
            {
                ReportOutOfMemory(cx);
                return false;
            }
        }
        *sn++ = (jssrcnote)(SN_4BYTE_OFFSET_FLAG | (offset >> 24));
        *sn++ = (jssrcnote)(offset >> 16);
        *sn++ = (jssrcnote)(offset >> 8);
    }
    *sn = (jssrcnote)offset;
    return true;
}

template <typename EmitProvider>
SourceNotesEmitHandlerImpl<EmitProvider>::addToSrcNoteDelta(jssrcnote* sn, ptrdiff_t delta)
{
    /*
     * Called only from finishTakingSrcNotes to add to main script note
     * deltas, and only by a small positive amount.
     */
    MOZ_ASSERT(current == &main);
    MOZ_ASSERT((unsigned) delta < (unsigned) SN_XDELTA_LIMIT);

    ptrdiff_t base = SN_DELTA(sn);
    ptrdiff_t limit = SN_IS_XDELTA(sn) ? SN_XDELTA_LIMIT : SN_DELTA_LIMIT;
    ptrdiff_t newdelta = base + delta;
    if (newdelta < limit) {
        SN_SET_DELTA(sn, newdelta);
    } else {
        jssrcnote xdelta;
        SN_MAKE_XDELTA(&xdelta, delta);
        if (!mainNotes.notes.insert(sn, xdelta))
            return false;
    }
    return true;
}

template <typename EmitProvider>
bool
SourceNotesEmitHandlerImpl<EmitProvider>::finishTakingSrcNotes(StmtInfoBCE* topStmt,
                                                               uint32_t* countOut)
{
    MOZ_ASSERT(currentNotes == &mainNotes);

    unsigned prologCount = prologNotes.notes.length();
    if (prologCount && prologNotes.currentLine != firstLine) {
        switchToProlog();
        if (!newSrcNote2(SRC_SETLINE, ptrdiff_t(firstLine), topStmt))
            return false;
        switchToMain();
    } else {
        /*
         * Either no prolog srcnotes, or no line number change over prolog.
         * We don't need a SRC_SETLINE, but we may need to adjust the offset
         * of the first main note, by adding to its delta and possibly even
         * prepending SRC_XDELTA notes to it to account for prolog bytecodes
         * that came at and after the last annotated bytecode.
         */
        ptrdiff_t offset = prologOffset() - prolog.lastNoteOffset;
        MOZ_ASSERT(offset >= 0);
        if (offset > 0 && mainNotes.notes.length() != 0) {
            /* NB: Use as much of the first main note's delta as we can. */
            jssrcnote* sn = mainNotes.notes.begin();
            ptrdiff_t delta = SN_IS_XDELTA(sn)
                            ? SN_XDELTA_MASK - (*sn & SN_XDELTA_MASK)
                            : SN_DELTA_MASK - (*sn & SN_DELTA_MASK);
            if (offset < delta)
                delta = offset;
            for (;;) {
                if (!addToSrcNoteDelta(sn, delta))
                    return false;
                offset -= delta;
                if (offset == 0)
                    break;
                delta = Min(offset, SN_XDELTA_MASK);
                sn = mainNotes.notes.begin();
            }
        }
    }

    // The prolog count might have changed, so we can't reuse prologCount.
    // The + 1 is to account for the final SN_MAKE_TERMINATOR that is appended
    // when the notes are copied to their final destination by CopySrcNotes.
    *countOut = prologNotes.notes.length() + mainNotes.notes.length() + 1;
    return true;
}

template <typename EmitProvider>
void
SourceNotesEmitHandlerImpl<EmitProvider>::copySrcNotes(jssrcnote* destination, uint32_t nsrcnotes)
{
    unsigned prologCount = prologNotes.notes.length();
    unsigned mainCount = mainNotes.notes.length();
    unsigned totalCount = prologCount + mainCount;
    MOZ_ASSERT(totalCount == nsrcnotes - 1);
    if (prologCount)
        PodCopy(destination, prologNotes.notes.begin(), prologCount);
    PodCopy(destination + prologCount, mainNotes.notes.begin(), mainCount);
    SN_MAKE_TERMINATOR(&destination[totalCount]);
}

template class frontend::SourceNotesEmitHandlerImpl<FullEmitHandler>;
template class frontend::SourceNotesEmitHandlerImpl<SourceNotesOnlyEmitHandler>;

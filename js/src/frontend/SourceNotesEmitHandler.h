/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef frontend_SourceNotesEmitHandler_h
#define frontend_SourceNotesEmitHandler_h

#include "frontend/SourceNotes.h"

namespace js {
namespace frontend {

typedef Vector<jssrcnote, 0> SrcNotesVector;

static inline size_t Emit1Length() { return 1; }
static inline size_t Emit2Length() { return 2; }
static inline size_t Emit3Length() { return 3; }
static inline size_t EmitNLength(size_t extra) { return extra; }
static inline size_t EmitJumpLength() { return 5; }
static inline size_t EmitIndex32Length() { return 1 + UINT32_INDEX_LEN; }
static inline size_t EmitIndexOpLength(JSOp op) { return js_CodeSpec[op].length; }

template <typename EmitProvider>
struct SourceNotesEmitHandlerImpl
{
    struct NotesSection {
        SrcNotesVector notes;          /* source notes, see below */
        ptrdiff_t      lastNoteOffset; /* code offset for last source note */
        uint32_t       currentLine;    /* line number for tree-based srcnote gen */
        uint32_t       lastColumn;     /* zero-based column index on currentLine of
                                          last SRC_COLSPAN-annotated opcode */

        NotesSection(ExclusiveContext* cx, uint32_t lineNum)
          : notes(cx),
            lastNoteOffset(0),
            currentLine(lineNum),
            lastColumn(0)
        { }
    };

    NotesSection  prologNotes;
    NotesSection  mainNotes;
    NotesSection* currentNotes;

    SourceNotesHandler(ExclusiveContext* cx, uint32_t lineNum);

    void switchToMain()   { currentNotes = &mainNotes; }
    void switchToProlog() { currentNotes = &prologNotes; }

    ptrdiff_t offset() const { return static_cast<const EmitProvider*>(this)->offset(); }
    SrcNotesVector& notes() const { return currentNotes->notes; }
    ptrdiff_t lastNoteOffset() const { return currentNotes->lastNoteOffset; }
    unsigned currentLine() const { return currentNotes->currentLine; }
    unsigned lastColumn() const { return currentNotes->lastColumn; }

    bool emit1(JSOp op) {
        mozilla::DebugOnly<ptrdiff_t> startOffset = offset();
        bool rv = static_cast<EmitProvider*>(this)->emit1(op);
        MOZ_ASSERT(offset() - startOffset == Emit1Length());
        return rv;
    }
    bool emit2(JSOp op, jsbytecode op1) {
        mozilla::DebugOnly<ptrdiff_t> startOffset = offset();
        bool rv = static_cast<EmitProvider*>(this)->emit2(op, op1);
        MOZ_ASSERT(offset() - startOffset == Emit2Length());
        return rv;
    }
    bool emit3(JSOp op, jsbytecode op1, jsbytecode op2) {
        mozilla::DebugOnly<ptrdiff_t> startOffset = offset();
        bool rv = static_cast<EmitProvider*>(this)->emit3(op, op1, op2);
        MOZ_ASSERT(offset() - startOffset == Emit3Length());
        return rv;
    }
    bool emitN(JSOp op, size_t extra, ptrdiff_t* offset) {
        mozilla::DebugOnly<ptrdiff_t> startOffset = offset();
        bool rv = static_cast<EmitProvider*>(this)->emitN(op, extra, offset);
        MOZ_ASSERT(offset() - startOffset == EmitNLength(extra));
        return rv;
    }
    bool emitJump(JSOp op, ptrdiff_t off, ptrdiff_t* jumpOffset) {
        mozilla::DebugOnly<ptrdiff_t> startOffset = offset();
        bool rv = static_cast<EmitProvider*>(this)->emitJump(op, off, jumpOffset);
        MOZ_ASSERT(offset() - startOffset == EmitJumpLength());
        return rv;
    }
    bool emitIndex32(JSOp op, uint32_t index) {
        mozilla::DebugOnly<ptrdiff_t> startOffset = offset();
        bool rv = static_cast<EmitProvider*>(this)->emitIndex32(op, index);
        MOZ_ASSERT(offset() - startOffset == EmitIndex32Length());
        return rv;
    }
    bool emitIndexOp(JSOp op, uint32_t index) {
        mozilla::DebugOnly<ptrdiff_t> startOffset = offset();
        bool rv = static_cast<EmitProvider*>(this)->emitIndexOp(op, index);
        MOZ_ASSERT(offset() - startOffset == EmitIndexOpLength(op));
        return rv;
    }
    bool emitNewInit(JSProtoKey key) {
        mozilla::DebugOnly<ptrdiff_t> startOffset = offset();
        bool rv = static_cast<EmitProvider*>(this)->emitNewInit(key);
        MOZ_ASSERT(offset() - startOffset == EmitIndex32Length());
        return rv;
    }

    // Append a new source note of the given type (and therefore size) to the
    // notes dynamic array, updating noteCount. Return the new note's index
    // within the array pointed at by current->notes as outparam.
    bool newSrcNote(SrcNoteType type, unsigned* indexp = nullptr);
    bool newSrcNote2(SrcNoteType type, ptrdiff_t offset, StmtInfoBCE* topStmt,
                     unsigned* indexp = nullptr);
    bool newSrcNote3(SrcNoteType type, ptrdiff_t offset1, ptrdiff_t offset2,
                     StmtInfoBCE* topStmt, unsigned* indexp = nullptr);
    bool setSrcNoteOffset(unsigned index, unsigned which, ptrdiff_t offset, StmtInfoBCE* topStmt);

    // NB: this function can add at most one extra extended delta note.
    bool addToSrcNoteDelta(jssrcnote* sn, ptrdiff_t delta);

    bool finishTakingSrcNotes(StmtInfoBCE* topStmt, uint32_t* countOut);
    void copySrcNotes(jssrcnote* destination, uint32_t nsrcnotes);

    bool updateLineNumberNotes(uint32_t offset);
    bool updateSourceCoordNotes(uint32_t offset);

    // If high resolution source notes are requested (e.g., for profiling),
    // update source coords.
    bool updateHighResSourceCoordNotes(uint32_t offset) {
        if (MOZ_UNLIKELY(highResSourceNotes))
            return updateSourceCoordNotes(offset);
        return true;
    }
};

struct SourceNotesOnlyEmitHandler : public SourceNotesEmitHandler<SourceNotesOnlyEmitHandler>
{
    ptrdiff_t mainOffset;
    ptrdiff_t prologOffset;
    ptrdiff_t *currentOffset;

    SourceNotesOnlyEmitHandler(ExclusiveContext* cx, uint32_t linNum);

    ptrdiff_t offset() const { return *currentOffset; }

    void switchToMain() {
        SourceNotesEmitHandlerImpl<SourcesNoteOnlyEmitHandler>::switchToMain();
        currentOffset = &mainOffset;
    }
    void switchToProlog() {
        SourceNotesEmitHandlerImpl<SourcesNoteOnlyEmitHandler>::switchToProlog();
        currentOffset = &prologOffset;
    }

    bool emit1(JSOp op) {
        *currentOffset += Emit1Length();
        return true;
    }
    bool emit2(JSOp op, jsbytecode op1) {
        *currentOffset += Emit2Length();
        return true;
    }
    bool emit3(JSOp op, jsbytecode op1, jsbytecode op2) {
        *currentOffset += Emit3Length();
        return true;
    }
    bool emitN(JSOp op, size_t extra, ptrdiff_t* offset) {
        *currentOffset += EmitNLength(extra);
        return true;
    }
    bool emitJump(JSOp op, ptrdiff_t off, ptrdiff_t* jumpOffset) {
        *currentOffset += EmitJumpLength();
        return true;
    }
    bool emitIndex32(JSOp op, uint32_t index) {
        *currentOffset += EmitIndex32Length();
        return true;
    }
    bool emitIndexOp(JSOp op, uint32_t index) {
        *currentOffset += EmitIndexOpLength(op);
        return true;
    }
    bool emitNewInit(JSProtoKey key) {
        *currentOffset += EmitIndex32Length();
        return true;
    }
};

} // frontend
} // js

#endif /* frontend_SourceNotesEmitHandler_h */

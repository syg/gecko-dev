/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef frontend_FullEmitHandler_h
#define frontend_FullEmitHandler_h

#include "frontend/SourceNotesEmitHandler.h"

namespace js {
namespace frontend {

template <typename EmitHandler>
class BytecodeEmitter;

typedef Vector<jsbytecode, 0> BytecodeVector;

struct FullEmitHandler : public SourceNotesEmitHandlerImpl<FullEmitHandler>
{
    BytecodeVector  prolog;
    BytecodeVector  main;
    BytecodeVector* current;

    FullEmitHandler(ExclusiveContext* cx, uint32_t lineNum);

    BytecodeVector& code() const { return current->code; }
    jsbytecode* code(ptrdiff_t offset) const { return current->code.begin() + offset; }
    ptrdiff_t offset() const { return current->code.end() - current->code.begin(); }
    ptrdiff_t prologOffset() const { return prolog.code.end() - prolog.code.begin(); }

    void switchToMain() {
        SourceNotesEmitHandler<FullEmitHandler>::switchToMain();
        current = &main;
    }
    void switchToProlog() {
        SourceNotesEmitHandler<FullEmitHandler>::switchToProlog();
        current = &prolog;
    }
};

} // frontend
} // js

#endif /* frontend_FullEmitHandler_h */

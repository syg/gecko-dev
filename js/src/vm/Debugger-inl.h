/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef vm_Debugger_inl_h
#define vm_Debugger_inl_h

#include "vm/Debugger.h"

#include "vm/Stack-inl.h"

inline bool
js::Debugger::onLeaveFrame(JSContext *cx, AbstractFramePtr frame, bool ok)
{
    MOZ_ASSERT_IF(frame.script()->isDebuggee(), frame.isDebuggee());
    /* Traps must be cleared from eval frames, see slowPathOnLeaveFrame. */
    mozilla::DebugOnly<bool> evalTraps = frame.isEvalFrame() &&
                                         frame.script()->hasAnyBreakpointsOrStepMode();
    MOZ_ASSERT_IF(evalTraps, frame.isDebuggee());
    if (frame.isDebuggee())
        ok = slowPathOnLeaveFrame(cx, frame, ok);
    return ok;
}

/* static */ inline js::Debugger *
js::Debugger::fromJSObject(JSObject *obj)
{
    JS_ASSERT(js::GetObjectClass(obj) == &jsclass);
    return (Debugger *) obj->getPrivate();
}

JSTrapStatus
js::Debugger::onEnterFrame(JSContext *cx, AbstractFramePtr frame, MutableHandleValue vp)
{
    MOZ_ASSERT_IF(frame.script()->isDebuggee(), frame.isDebuggee());
    if (!frame.isDebuggee())
        return JSTRAP_CONTINUE;
    return slowPathOnEnterFrame(cx, frame, vp);
}

JSTrapStatus
js::Debugger::onDebuggerStatement(JSContext *cx, AbstractFramePtr frame, MutableHandleValue vp)
{
    MOZ_ASSERT_IF(frame.script()->isDebuggee(), frame.isDebuggee());
    return frame.isDebuggee()
           ? dispatchHook(cx, vp, OnDebuggerStatement)
           : JSTRAP_CONTINUE;
}

JSTrapStatus
js::Debugger::onExceptionUnwind(JSContext *cx, AbstractFramePtr frame, MutableHandleValue vp)
{
    MOZ_ASSERT_IF(frame.script()->isDebuggee(), frame.isDebuggee());
    return frame.isDebuggee()
           ? dispatchHook(cx, vp, OnExceptionUnwind)
           : JSTRAP_CONTINUE;
}

#endif /* vm_Debugger_inl_h */

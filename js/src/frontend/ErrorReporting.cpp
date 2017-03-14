/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "frontend/ErrorReporting.h"

#include <stdarg.h>

#include "jsapi.h"
#include "jscntxt.h"

using namespace js;
using namespace js::frontend;

using mozilla::Maybe;

static void
ReportCompileErrorVA(JSContext* cx, UniquePtr<JSErrorNotes> notes, Maybe<uint32_t> offset,
                     unsigned flags, unsigned errorNumber, va_list args)
{
}

void
ReportError(JSContext* cx, Maybe<uint32_t> offset, unsigned errorNumber, ...)
{
    va_list args;
    va_start(args, errorNumber);
    ReportCompileErrorVA(cx, nullptr, offset, JSREPORT_ERROR, errorNumber, args);
    va_end(args);
}

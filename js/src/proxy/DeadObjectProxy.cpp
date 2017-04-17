/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "proxy/DeadObjectProxy.h"

#include "jsapi.h"
#include "jsfun.h" // XXXefaust Bug 1064662

#include "proxy/ScriptedProxyHandler.h"
#include "vm/ProxyObject.h"

using namespace js;
using namespace js::gc;

static void
ReportDead(JSContext *cx)
{
    JS_ReportErrorNumberASCII(cx, GetErrorMessage, nullptr, JSMSG_DEAD_OBJECT);
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::getOwnPropertyDescriptor(JSContext* cx, HandleObject wrapper, HandleId id,
                                          MutableHandle<PropertyDescriptor> desc) const
{
    ReportDead(cx);
    return false;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::defineProperty(JSContext* cx, HandleObject wrapper, HandleId id,
                                Handle<PropertyDescriptor> desc,
                                ObjectOpResult& result) const
{
    ReportDead(cx);
    return false;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::ownPropertyKeys(JSContext* cx, HandleObject wrapper,
                                 AutoIdVector& props) const
{
    ReportDead(cx);
    return false;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::delete_(JSContext* cx, HandleObject wrapper, HandleId id,
                         ObjectOpResult& result) const
{
    ReportDead(cx);
    return false;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::getPrototype(JSContext* cx, HandleObject proxy, MutableHandleObject protop) const
{
    protop.set(nullptr);
    return true;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::getPrototypeIfOrdinary(JSContext* cx, HandleObject proxy, bool* isOrdinary,
                                        MutableHandleObject protop) const
{
    *isOrdinary = false;
    return true;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::preventExtensions(JSContext* cx, HandleObject proxy, ObjectOpResult& result) const
{
    ReportDead(cx);
    return false;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::isExtensible(JSContext* cx, HandleObject proxy, bool* extensible) const
{
    // This is kind of meaningless, but dead-object semantics aside,
    // [[Extensible]] always being true is consistent with other proxy types.
    *extensible = true;
    return true;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::call(JSContext* cx, HandleObject wrapper, const CallArgs& args) const
{
    ReportDead(cx);
    return false;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::construct(JSContext* cx, HandleObject wrapper, const CallArgs& args) const
{
    ReportDead(cx);
    return false;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::nativeCall(JSContext* cx, IsAcceptableThis test, NativeImpl impl,
                            const CallArgs& args) const
{
    ReportDead(cx);
    return false;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::hasInstance(JSContext* cx, HandleObject proxy, MutableHandleValue v,
                             bool* bp) const
{
    ReportDead(cx);
    return false;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::getBuiltinClass(JSContext* cx, HandleObject proxy, ESClass* cls) const
{
    ReportDead(cx);
    return false;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::isArray(JSContext* cx, HandleObject obj, JS::IsArrayAnswer* answer) const
{
    ReportDead(cx);
    return false;
}

template <bool IsConstructor>
const char*
DeadObjectProxy<IsConstructor>::className(JSContext* cx, HandleObject wrapper) const
{
    return "DeadObject";
}

template <bool IsConstructor>
JSString*
DeadObjectProxy<IsConstructor>::fun_toString(JSContext* cx, HandleObject proxy, unsigned indent) const
{
    ReportDead(cx);
    return nullptr;
}

template <bool IsConstructor>
bool
DeadObjectProxy<IsConstructor>::regexp_toShared(JSContext* cx, HandleObject proxy,
                                 MutableHandle<RegExpShared*> shared) const
{
    ReportDead(cx);
    return false;
}

template <>
const char DeadObjectProxy<true>::family = 0;
template <>
const char DeadObjectProxy<false>::family = 0;

bool
js::IsDeadProxyObject(JSObject* obj)
{
    return IsDerivedProxyObject(obj, DeadObjectProxy<true>::singleton()) ||
        IsDerivedProxyObject(obj, DeadObjectProxy<false>::singleton());
}

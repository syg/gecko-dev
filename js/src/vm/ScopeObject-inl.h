/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef vm_ScopeObject_inl_h
#define vm_ScopeObject_inl_h

#include "vm/ScopeObject.h"
#include "frontend/SharedContext.h"

#include "jsobjinlines.h"

#include "vm/TypeInference-inl.h"

namespace js {

inline LexicalEnvironmentObject&
NearestEnclosingExtensibleLexicalEnvironment(JSObject* env)
{
    while (!IsExtensibleLexicalEnvironment(env))
        env = env->enclosingScope();
    return env->as<LexicalEnvironmentObject>();
}

inline void
ScopeObject::setAliasedVar(JSContext* cx, ScopeCoordinate sc, PropertyName* name, const Value& v)
{
    MOZ_ASSERT(is<LexicalScopeBase>() || is<ClonedBlockObject>());
    JS_STATIC_ASSERT(CallObject::RESERVED_SLOTS == ClonedBlockObject::RESERVED_SLOTS);

    // name may be null if we don't need to track side effects on the object.
    MOZ_ASSERT_IF(isSingleton(), name);

    if (isSingleton()) {
        MOZ_ASSERT(name);
        AddTypePropertyId(cx, this, NameToId(name), v);

        // Keep track of properties which have ever been overwritten.
        if (!getSlot(sc.slot()).isUndefined()) {
            Shape* shape = lookup(cx, name);
            shape->setOverwritten();
        }
    }

    setSlot(sc.slot(), v);
}

inline void
EnvironmentObject::setAliasedBinding(JSContext* cx, uint32_t slot, PropertyName* name,
                                     const Value& v)
{
    if (isSingleton()) {
        MOZ_ASSERT(name);
        AddTypePropertyId(cx, this, NameToId(name), v);

        // Keep track of properties which have ever been overwritten.
        if (!getSlot(slot).isUndefined()) {
            Shape* shape = lookup(cx, name);
            shape->setOverwritten();
        }
    }

    setSlot(slot, v);
}

inline void
EnvironmentObject::setAliasedBinding(JSContext* cx, ScopeCoordinate sc, PropertyName* name,
                                     const Value& v)
{
    setAliasedBinding(cx, sc.slot(), name, v);
}

inline void
EnvironmentObject::setAliasedBinding(JSContext* cx, const BindingIter& bi, const Value& v)
{
    MOZ_ASSERT(bi.location().kind() == BindingLocation::Kind::Environment);
    setAliasedBinding(cx, bi.location().slot(), bi.name()->asPropertyName(), v);
}

inline void
LexicalScopeBase::setAliasedVar(JSContext* cx, const BindingIter& bi, const Value& v)
{
    MOZ_ASSERT(bi.location().kind() == BindingLocation::Kind::Environment);
    setSlot(bi.location().slot(), v);
    if (isSingleton())
        AddTypePropertyId(cx, this, NameToId(bi.name()->asPropertyName()), v);
}

inline void
LexicalScopeBase::setAliasedVarFromArguments(JSContext* cx, const Value& argsValue, jsid id,
                                             const Value& v)
{
    setSlot(ArgumentsObject::SlotFromMagicScopeSlotValue(argsValue), v);
    if (isSingleton())
        AddTypePropertyId(cx, this, id, v);
}

inline void
CallObject::setAliasedFormalFromArguments(JSContext* cx, const Value& argsValue, jsid id,
                                          const Value& v)
{
    setSlot(ArgumentsObject::SlotFromMagicScopeSlotValue(argsValue), v);
    if (isSingleton())
        AddTypePropertyId(cx, this, id, v);
}

}  /* namespace js */

inline JSObject*
JSObject::enclosingScope() const
{
    if (is<js::EnvironmentObject>())
        return &as<js::EnvironmentObject>().enclosingEnvironment();

    if (is<js::DebugEnvironmentProxy>())
        return &as<js::DebugEnvironmentProxy>().enclosingEnvironment();

    if (is<js::GlobalObject>())
        return nullptr;

    MOZ_ASSERT_IF(is<JSFunction>(), as<JSFunction>().isInterpreted());
    return &global();
}

#endif /* vm_ScopeObject_inl_h */

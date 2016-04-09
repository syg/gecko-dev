/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef frontend_ParseScope_h
#define frontend_ParseScope_h

#include "jscntxt.h"
#include "ds/LifoAlloc.h"
#include "gc/Rooting.h"
#include "js/HashTable.h"
#include "mozilla/Attributes.h"
#include "mozilla/Maybe.h"
#include "vm/String.h"

namespace js {

namespace frontend {

enum class BindingKind {
    Argument,
    Var,
    Let,
    Constant,
    Import
};

class ParseScope
{
    using AtomMap = HashMap<JSAtom*,
                            BindingKind,
                            DefaultHasher<JSAtom*>,
                            LifoAllocPolicy<Fallible>>;

    using AtomSet = HashSet<JSAtom*,
                            DefaultHasher<JSAtom*>,
                            LifoAllocPolicy<Fallible>>;

    // Names bound by this scope.
    AtomMap bindings_;

    // Names used in this scope.
    AtomSet uses_;

  public:
    explicit ParseScope(LifoAlloc& alloc)
      : bindings_(alloc), uses_(alloc)
    { }

    bool init() {
        return bindings_.init() && uses_.init();
    }

    mozilla::Maybe<BindingKind> lookupBinding(JSAtom* name) const {
        if (AtomMap::Ptr p = bindings_.lookup(name))
            return mozilla::Some(p->value());
        return mozilla::Nothing();
    }

    bool hasBinding(JSAtom* name) const {
        return lookupBinding(name).isSome();
    }

    bool addBinding(JSAtom* name, BindingKind kind) {
        MOZ_ASSERT(!hasBinding(name));
        return bindings_.put(name, kind);
    }

    bool addUse(JSAtom* name) {
        return uses_.put(name);
    }

#ifdef DEBUG
    void dump(ExclusiveContext* cx);
#endif
};

class ParseScopeStmtInfo;

class MOZ_STACK_CLASS ParseStmtInfo
{
  public:
    enum Kind {
        Label,
        If,
        Else,
        Destructuring,
        BodyLevel,
        Block,
        Switch,
        With,
        Catch,
        Try,
        Finally,
        DoLoop,
        ForLoop,
        ForInLoop,
        ForOfLoop,
        WhileLoop,
        Spread
    };

  private:
    Kind kind_;

    ParseStmtInfo** stack_;

    // The enclosing intra-script statement, nullptr if this is the body-level
    // statement.
    ParseStmtInfo* enclosing_;

    // The label if kind == Label, nullptr otherwise.
    RootedAtom label_;

    // Used for simplified dominance computation.
    uint32_t blockId_;

  protected:
    enum CheckIsScope { IsScope = true, IsNotScope = false };
    template <typename ParseContext>
    ParseStmtInfo(ParseContext& pc, Kind kind, CheckIsScope checkIsScope)
      : kind_(kind),
        stack_(&pc.stmtStack2),
        enclosing_(pc.stmtStack2),
        label_(pc.sc->context),
        blockId_(0)
    {
        MOZ_ASSERT(checkIsScope == isScope(),
                   "use ParseScopeStmtInfo for statements that introduce scopes");
        *stack_ = this;
    }

  public:
    template <typename ParseContext>
    ParseStmtInfo(ParseContext& pc, Kind kind)
      : ParseStmtInfo(pc, kind, IsNotScope)
    { }

    ~ParseStmtInfo() {
        MOZ_ASSERT(*stack_ == this);
        *stack_ = enclosing_;
    }

    ParseStmtInfo* enclosing() const {
        return enclosing_;
    }

    Kind kind() const {
        return kind_;
    }

    HandleAtom label() const {
        return label_;
    }

    bool isScope() const {
        return kind_ == BodyLevel ||
               kind_ == Block ||
               kind_ == Switch ||
               kind_ == With ||
               kind_ == Catch ||
               kind_ == Try ||
               kind_ == Finally;
    }

    inline ParseScopeStmtInfo& asScopeStmt();
};

class MOZ_STACK_CLASS ParseScopeStmtInfo : public ParseStmtInfo
{
    ParseScope scope_;

    ExclusiveContext* cx_;

  public:
    template <typename ParseContext>
    ParseScopeStmtInfo(ParseContext& context, LifoAlloc& alloc, Kind kind)
      : ParseStmtInfo(context, kind, IsScope),
        scope_(alloc),
        cx_(context.sc->context)
    { }

    ~ParseScopeStmtInfo() {
#ifdef DEBUG
        scope_.dump(cx_);
#endif
    }

    bool init() {
        return scope_.init();
    }

    ParseScope& scope() {
        return scope_;
    }
};

inline ParseScopeStmtInfo&
ParseStmtInfo::asScopeStmt()
{
    MOZ_ASSERT(isScope());
    return static_cast<ParseScopeStmtInfo&>(*this);
}

} // namespace frontend
} // namespace js

#endif // frontend_ParseScope_h

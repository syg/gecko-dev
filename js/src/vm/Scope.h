/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef vm_Scope_h
#define vm_Scope_h

#include "jsobj.h"
#include "jsopcode.h"

#include "mozilla/Maybe.h"

#include "gc/Heap.h"
#include "gc/Policy.h"
#include "js/UbiNode.h"

namespace js {

enum class BindingKind : uint8_t
{
    FormalParameter,
    Var,
    Let,
    Const
};

static inline bool
BindingKindIsLexical(BindingKind kind)
{
    return kind == BindingKind::Let || kind == BindingKind::Const;
}

enum class ScopeKind : uint8_t
{
    Function,
    Lexical,
    Catch,
    With,
    Eval,
    StrictEval,
    Global,
    NonSyntactic,
    Module,
};

static inline bool
ScopeKindCanHaveBindings(ScopeKind kind)
{
    return kind == ScopeKind::Function ||
           kind == ScopeKind::Lexical ||
           kind == ScopeKind::Catch ||
           kind == ScopeKind::Eval ||
           kind == ScopeKind::StrictEval ||
           kind == ScopeKind::Global ||
           kind == ScopeKind::NonSyntactic;
}

static inline bool
ScopeKindCanHaveEnvironment(ScopeKind kind)
{
    return kind == ScopeKind::Function ||
           kind == ScopeKind::Lexical ||
           kind == ScopeKind::Catch ||
           kind == ScopeKind::StrictEval;
}

const char* BindingKindString(BindingKind kind);
const char* ScopeKindString(ScopeKind kind);

class BindingName
{
    // A JSAtom* with its low bit used as a tag for whether it is closed over
    // (i.e., exists in the environment shape).
    uintptr_t bits_;

    static const uintptr_t ClosedOverFlag = 0x1;
    static const uintptr_t FlagMask = 0x1;

  public:
    BindingName()
      : bits_(0)
    { }

    BindingName(JSAtom* name, bool closedOver)
      : bits_(uintptr_t(name) | (closedOver ? ClosedOverFlag : 0x0))
    { }

    JSAtom* name() const {
        return (JSAtom*)(bits_ & ~FlagMask);
    }

    bool closedOver() const {
        return bits_ & ClosedOverFlag;
    }
};

class BindingLocation
{
  public:
    enum class Kind {
        Global,
        Argument,
        Frame,
        Environment
    };

  private:
    Kind kind_;
    uint32_t slot_;

    BindingLocation(Kind kind, uint32_t slot)
      : kind_(kind),
        slot_(slot)
    { }

  public:
    static BindingLocation Global() {
        return BindingLocation(Kind::Global, UINT32_MAX);
    }

    static BindingLocation Argument(uint16_t slot) {
        return BindingLocation(Kind::Argument, slot);
    }

    static BindingLocation Frame(uint32_t slot) {
        return BindingLocation(Kind::Frame, slot);
    }

    static BindingLocation Environment(uint32_t slot) {
        return BindingLocation(Kind::Environment, slot);
    }

    Kind kind() const {
        return kind_;
    }

    uint32_t slot() const {
        MOZ_ASSERT(kind_ != Kind::Argument && kind_ != Kind::Global);
        return slot_;
    }

    uint16_t argumentSlot() const {
        MOZ_ASSERT(kind_ == Kind::Argument);
        return mozilla::AssertedCast<uint16_t>(slot_);
    }
};

class Scope : public js::gc::TenuredCell
{
    friend class GCMarker;

    // The kind determines data_.
    ScopeKind kind_;

    // The enclosing scope or nullptr.
    GCPtrScope enclosing_;

  protected:
    uintptr_t data_;

    Scope(ScopeKind kind, Scope* enclosing, uintptr_t data)
      : kind_(kind),
        enclosing_(enclosing),
        data_(data)
    { }

    static Scope* create(ExclusiveContext* cx, ScopeKind kind, Scope* enclosing,
                         uintptr_t data);

  public:
    static const JS::TraceKind TraceKind = JS::TraceKind::Scope;

    template <typename T>
    bool is() const {
        return kind_ == T::classScopeKind_;
    }

    template <typename T>
    T& as() {
        MOZ_ASSERT(is<T>());
        return *static_cast<T*>(this);
    }

    ScopeKind kind() const {
        return kind_;
    }

    Scope* enclosing() const {
        return enclosing_;
    }

    bool hasBindings() const {
        return !ScopeKindCanHaveBindings(kind_);
    }

    void traceChildren(JSTracer* trc);
    void finalize(FreeOp* fop);
};

class LexicalScope : public Scope
{
    friend class BindingIter;

  public:
    // Data is public because it is created by the frontend. See
    // Parser<FullParseHandler>::newLexicalScopeData.
    struct Data
    {
        // Bindings are sorted by kind in both frames and environments.
        //
        //   lets - [0, varStart)
        // consts - [varStart, length)
        uint32_t constStart;
        uint32_t length;

        // Frame slots [0, nextFrameSlot) are live when this is the innermost
        // scope.
        uint32_t nextFrameSlot;

        // If there are any aliased bindings, the shape for the
        // LexicalEnvironment to create. Otherwise nullptr.
        GCPtrShape environmentShape;

        // The array of tagged JSAtom* names, allocated beyond the end of the
        // struct.
        BindingName names[1];
    };

    static size_t sizeOfData(uint32_t length) {
        MOZ_ASSERT(length > 0);
        return sizeof(Data) + (length - 1) * sizeof(BindingName);
    }

    static LexicalScope* create(ExclusiveContext* cx, ScopeKind kind, Data* data,
                                uint32_t firstFrameSlot, Scope* enclosing);

  protected:
    Data& data() {
        return *reinterpret_cast<Data*>(data_);
    }

    const Data& data() const {
        return *reinterpret_cast<Data*>(data_);
    }

    static uint32_t computeNextFrameSlot(Scope* start);

  public:
    uint32_t computeFirstFrameSlot() const {
        return computeNextFrameSlot(enclosing());
    }

    uint32_t nextFrameSlot() const {
        return data_ ? data().nextFrameSlot : LOCALNO_LIMIT;
    }

    Shape* environmentShape() const {
        return data_ ? data().environmentShape : nullptr;
    }
};

template <>
inline bool
Scope::is<LexicalScope>() const
{
    return kind_ == ScopeKind::Lexical || kind_ == ScopeKind::Catch;
}

class FunctionScope : public Scope
{
    friend class BindingIter;
    friend class SimpleFormalParameterIter;
    friend class Scope;
    static const ScopeKind classScopeKind_ = ScopeKind::Function;

  public:
    // Data is public because it is created by the frontend. See
    // Parser<FullParseHandler>::newFunctionScopeData.
    struct Data
    {
        // Bindings are sorted by kind in both frames and environments.
        //
        // Simple formal parameter names are those without default expressions
        // or destructuring, i.e. those that may be referred to by argument
        // slots.
        //
        // simple formals - [0, nonSimpleFormalStart)
        //  other formals - [nonSimpleParamStart, varStart)
        //           vars - [varStart, length)
        uint16_t nonSimpleFormalStart;
        uint16_t varStart;
        uint32_t length;

        // Frame slots [0, nextFrameSlot) are live when this is the innermost
        // scope.
        uint32_t nextFrameSlot;

        // The canonical function of the scope, as during a scope walk we
        // often query properties of the JSFunction (e.g., is the function an
        // arrow).
        GCPtrFunction canonicalFunction;

        // If there are any aliased bindings, the shape for the
        // CallEnvironment. Otherwise nullptr.
        GCPtrShape environmentShape;

        // The array of tagged JSAtom* names, allocated beyond the end of the
        // struct.
        BindingName names[1];
    };

    static size_t sizeOfData(uint32_t length) {
        MOZ_ASSERT(length > 0);
        return sizeof(Data) + (length - 1) * sizeof(BindingName);
    }

    static FunctionScope* create(ExclusiveContext* cx, Data* data, JSFunction* fun,
                                 Scope* enclosing);

  private:
    Data& data() {
        return *reinterpret_cast<Data*>(data_);
    }

    const Data& data() const {
        return *reinterpret_cast<Data*>(data_);
    }

  public:
    // Unlike other Scopes, FunctionScopes always has data_ to hold the
    // canonical function.

    uint32_t nextFrameSlot() const {
        return data().nextFrameSlot;
    }

    JSFunction* canonicalFunction() const {
        return data().canonicalFunction;
    }

    JSScript* script() const;

    Shape* environmentShape() const {
        return data().environmentShape;
    }
};

// Scope corresponding to both the global object scope and the global lexical
// scope.
//
// Both are extensible and are singletons across <script> tags and are super
// weird, so these scopes similarly a fragment of the names in global
// scope. In other words, two global scripts may have two different
// GlobalScopes despite having the same GlobalObject.
//
// A global scope may be non-syntactic, meaning the scope is not a
// GlobalObject but some other kind of object created by the embedding. This
// distinction is important for optimizations.
class GlobalScope : public Scope
{
    friend class BindingIter;

  public:
    // Data is public because it is created by the frontend. See
    // Parser<FullParseHandler>::newGlobalScopeData.
    struct Data
    {
        // Bindings are sorted by kind.
        //
        //   vars - [0, letStart)
        //   lets - [letStart, constStart)
        // consts - [constStart, length)
        uint32_t letStart;
        uint32_t constStart;
        uint32_t length;

        // The array of tagged JSAtom* names, allocated beyond the end of the
        // struct.
        BindingName names[1];
    };

    static size_t sizeOfData(uint32_t length) {
        MOZ_ASSERT(length > 0);
        return sizeof(Data) + (length - 1) * sizeof(BindingName);
    }

    static GlobalScope* create(ExclusiveContext* cx, ScopeKind kind, Data* data);

    bool isNonSyntactic() const {
        return kind() == ScopeKind::NonSyntactic;
    }

  private:
    Data& data() {
        return *reinterpret_cast<Data*>(data_);
    }
};

template <>
inline bool
Scope::is<GlobalScope>() const
{
    return kind_ == ScopeKind::Global || kind_ == ScopeKind::NonSyntactic;
}

class WithScope : public Scope
{
    friend class Scope;
    static const ScopeKind classScopeKind_ = ScopeKind::With;

  public:
    static WithScope* create(ExclusiveContext* cx, Scope* enclosing);
};

class EvalScope : public Scope
{
    friend class BindingIter;
    friend class Scope;
    static const ScopeKind classScopeKind_ = ScopeKind::Eval;

  public:
    // Data is public because it is created by the frontend. See
    // Parser<FullParseHandler>::newEvalScopeData.
    struct Data
    {
        // All bindings in an eval script are 'var' bindings. The implicit
        // lexical scope around the eval is present regardless of strictness
        // and is its own LexicalScope.
        uint32_t length;

        // Frame slots [0, nextFrameSlot) are live when this is the innermost
        // scope.
        uint32_t nextFrameSlot;

        // Strict eval scopes have their own var scopes. If there are any
        // aliased bindings, the shape for the CallEnvironment. Otherwise
        // nullptr.
        GCPtrShape environmentShape;

        // The array of tagged JSAtom* names, allocated beyond the end of the
        // struct.
        BindingName names[1];
    };

    static size_t sizeOfData(uint32_t length) {
        MOZ_ASSERT(length > 0);
        return sizeof(Data) + (length - 1) * sizeof(BindingName);
    }

    static EvalScope* create(ExclusiveContext* cx, ScopeKind kind, Data* data, Scope* enclosing);

  private:
    Data& data() {
        return *reinterpret_cast<Data*>(data_);
    }

    const Data& data() const {
        return *reinterpret_cast<Data*>(data_);
    }

  public:
    uint32_t nextFrameSlot() const {
        return data_ ? data().nextFrameSlot : LOCALNO_LIMIT;
    }

    Shape* environmentShape() const {
        return data_ ? data().environmentShape : nullptr;
    }

    bool strict() const {
        return kind() == ScopeKind::StrictEval;
    }
};

class BindingIter
{
    // Bindings are sorted by kind. Because different Scopes have differently
    // laid out Data for packing, BindingIter must handle all binding kinds.
    //
    // simple formals - [0, nonSimpleFormalStart)
    //  other formals - [nonSimpleParamStart, varStart)
    //           vars - [varStart, letStart)
    //           lets - [letStart, constStart)
    //         consts - [constStart, length)
    uint16_t nonSimpleFormalStart_;
    uint16_t varStart_;
    uint32_t letStart_;
    uint32_t constStart_;

    uint32_t index_;

    bool hasSlots_;
    uint16_t argumentSlot_;
    uint32_t frameSlot_;
    uint32_t environmentSlot_;

    uint32_t length_;
    BindingName* names_;

    void init(uint16_t nonSimpleFormalStart, uint16_t varStart,
              uint32_t letStart, uint32_t constStart, bool hasSlots,
              uint32_t firstFrameSlot, uint32_t firstEnvironmentSlot,
              BindingName* names, uint32_t length)
    {
        nonSimpleFormalStart_ = nonSimpleFormalStart;
        varStart_ = varStart;
        letStart_ = letStart;
        constStart_ = constStart;
        index_ = 0;
        hasSlots_ = hasSlots;
        argumentSlot_ = 0;
        frameSlot_ = firstFrameSlot;
        environmentSlot_ = firstEnvironmentSlot;
        length_ = length;
        names_ = names;
    }

    void init(LexicalScope::Data& data, uint32_t firstFrameSlot);
    void init(FunctionScope::Data& data);
    void init(GlobalScope::Data& data);
    void init(EvalScope::Data& data, bool strict);

  public:
    BindingIter(Scope* scope) {
        if (!scope->hasBindings()) {
            init(0, 0, 0, 0, false, 0, 0, nullptr, 0);
            return;
        }

        switch (scope->kind()) {
          case ScopeKind::Lexical:
          case ScopeKind::Catch:
            init(scope->as<LexicalScope>().data(),
                 scope->as<LexicalScope>().computeFirstFrameSlot());
            break;
          case ScopeKind::Function:
            init(scope->as<FunctionScope>().data());
            break;
          case ScopeKind::Eval:
            init(scope->as<EvalScope>().data(), scope->kind() == ScopeKind::StrictEval);
            break;
          case ScopeKind::Global:
            init(scope->as<GlobalScope>().data());
            break;
          default:
            MOZ_CRASH("Scope cannot have bindings.");
        }
    }

    BindingIter(LexicalScope::Data& data, uint32_t firstFrameSlot) {
        init(data, firstFrameSlot);
    }

    explicit BindingIter(FunctionScope::Data& data) {
        init(data);
    }

    explicit BindingIter(GlobalScope::Data& data) {
        init(data);
    }

    explicit BindingIter(EvalScope::Data& data, bool strict) {
        init(data, strict);
    }

    explicit BindingIter(JSScript* script);

    explicit BindingIter(const BindingIter& bi)
      : nonSimpleFormalStart_(bi.nonSimpleFormalStart_),
        varStart_(bi.varStart_),
        letStart_(bi.letStart_),
        constStart_(bi.constStart_),
        index_(bi.index_),
        hasSlots_(bi.hasSlots_),
        argumentSlot_(bi.argumentSlot_),
        frameSlot_(bi.frameSlot_),
        environmentSlot_(bi.environmentSlot_),
        length_(bi.length_),
        names_(bi.names_)
    { }

    bool done() const {
        return index_ == length_;
    }

    explicit operator bool() const {
        return !done();
    }

    void operator++(int) {
        MOZ_ASSERT(!done());
        if (hasSlots_) {
            if (index_ < nonSimpleFormalStart_)
                argumentSlot_++;
            if (closedOver())
                environmentSlot_++;
            else if (index_ >= nonSimpleFormalStart_)
                frameSlot_++;
        }
        index_++;
    }

    JSAtom* name() const {
        MOZ_ASSERT(!done());
        return names_[index_].name();
    }

    bool closedOver() const {
        MOZ_ASSERT(!done());
        return names_[index_].closedOver();
    }

    BindingLocation location() const {
        MOZ_ASSERT(!done());
        if (!hasSlots_)
            return BindingLocation::Global();
        if (closedOver())
            return BindingLocation::Environment(environmentSlot_);
        if (index_ < nonSimpleFormalStart_)
            return BindingLocation::Argument(argumentSlot_);
        return BindingLocation::Frame(frameSlot_);
    }

    BindingKind kind() const {
        MOZ_ASSERT(!done());
        if (index_ < varStart_)
            return BindingKind::FormalParameter;
        if (index_ < letStart_)
            return BindingKind::Var;
        if (index_ < constStart_)
            return BindingKind::Let;
        return BindingKind::Const;
    }

    bool isSimpleFormalParameter() const {
        MOZ_ASSERT(!done());
        return index_ < nonSimpleFormalStart_;
    }

    uint16_t simpleFormalParameterPosition() const {
        MOZ_ASSERT(isSimpleFormalParameter());
        return mozilla::AssertedCast<uint16_t>(index_);
    }

    uint32_t nextFrameSlot() const {
        MOZ_ASSERT(hasSlots_);
        return frameSlot_;
    }

    uint32_t nextEnvironmentSlot() const {
        MOZ_ASSERT(hasSlots_);
        return environmentSlot_;
    }

    void trace(JSTracer* trc);
};

class SimpleFormalParameterIter
{
    uint16_t index_;
    FunctionScope::Data& data_;
    JS::AutoAssertOnGC nogc;

  public:
    explicit SimpleFormalParameterIter(Scope* scope)
      : index_(0),
        data_(scope->as<FunctionScope>().data())
    { }

    explicit SimpleFormalParameterIter(JSScript* script);

    bool done() const {
        return index_ == data_.nonSimpleFormalStart;
    }

    explicit operator bool() const {
        return !done();
    }

    void operator++(int) {
        MOZ_ASSERT(!done());
        index_++;
    }

    uint16_t position() const {
        MOZ_ASSERT(!done());
        return index_;
    }

    JSAtom* name() const {
        MOZ_ASSERT(!done());
        return data_.names[index_].name();
    }

    bool closedOver() {
        MOZ_ASSERT(!done());
        return data_.names[index_].closedOver();
    }
};

class MOZ_STACK_CLASS ScopeIter
{
    HeapPtr<Scope*> scope_;

  public:
    explicit ScopeIter(Scope* scope)
      : scope_(scope)
    { }

    explicit ScopeIter(ScopeIter& si)
      : scope_(si.scope_)
    { }

    bool done() const {
        return !scope_;
    }

    explicit operator bool() const {
        return !done();
    }

    void operator++(int) {
        MOZ_ASSERT(!done());
        scope_ = scope_->enclosing();
    }

    Scope* scope() const {
        MOZ_ASSERT(!done());
        return scope_;
    }

    ScopeKind kind() const {
        MOZ_ASSERT(!done());
        return scope_->kind();
    }

    // Returns whether this scope has a syntactic environment (i.e., an
    // Environment that isn't a non-syntactic With or NonSyntacticVariables)
    // on the environment chain.
    bool hasSyntacticEnvironment() const;

    // If hasSyntacticEnvironment(), returns the shape of the environment.
    Shape* environmentShape() const;

    void trace(JSTracer* trc) {
        TraceEdge(trc, &scope_, "scope iter scope");
    }
};

// Starting at scope, count the number of scopes.
uint32_t ScopeChainLength(Scope* scope);

// Starting at scope, is there a scope of kind ScopeKind::NonSyntactic?
bool HasNonSyntacticScopeChain(Scope* scope);

} // namespace js

namespace JS {
namespace ubi {

template <>
struct Concrete<js::Scope> : TracerConcrete<js::Scope>
{
    Size size(mozilla::MallocSizeOf mallocSizeOf) const override;

  protected:
    explicit Concrete(js::Scope* ptr)
      : TracerConcrete<js::Scope>(ptr)
    { }

  public:
    static void construct(void* storage, js::Scope* ptr) {
        new (storage) Concrete(ptr);
    }
};

} // namespace ubi
} // namespace JS

#endif // vm_Scope_h

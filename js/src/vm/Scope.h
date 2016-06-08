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

namespace frontend {
class FunctionBox;
};

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
    ParameterDefaults,
    Lexical,
    Catch,
    With,
    Eval,
    StrictEval,
    Global,
    NonSyntactic,
    Module,
};

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

    bool operator==(const BindingLocation& other) const {
        return kind_ == other.kind_ && slot_ == other.slot_;
    }

    bool operator!=(const BindingLocation& other) const {
        return !operator==(other);
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

/*
 * A "scope coordinate" describes how to get from head of the scope chain to a
 * given lexically-enclosing variable. A scope coordinate has two dimensions:
 *  - hops: the number of scope objects on the scope chain to skip
 *  - slot: the slot on the scope object holding the variable's value
 */
class ScopeCoordinate
{
    uint32_t hops_;
    uint32_t slot_;

    /*
     * Technically, hops_/slot_ are SCOPECOORD_(HOPS|SLOT)_BITS wide.  Since
     * ScopeCoordinate is a temporary value, don't bother with a bitfield as
     * this only adds overhead.
     */
    static_assert(SCOPECOORD_HOPS_BITS <= 32, "We have enough bits below");
    static_assert(SCOPECOORD_SLOT_BITS <= 32, "We have enough bits below");

  public:
    explicit inline ScopeCoordinate(jsbytecode* pc)
      : hops_(GET_SCOPECOORD_HOPS(pc)), slot_(GET_SCOPECOORD_SLOT(pc + SCOPECOORD_HOPS_LEN))
    {
        MOZ_ASSERT(JOF_OPTYPE(JSOp(*pc)) == JOF_SCOPECOORD);
    }

    inline ScopeCoordinate() {}

    void setHops(uint32_t hops) { MOZ_ASSERT(hops < SCOPECOORD_HOPS_LIMIT); hops_ = hops; }
    void setSlot(uint32_t slot) { MOZ_ASSERT(slot < SCOPECOORD_SLOT_LIMIT); slot_ = slot; }

    uint32_t hops() const { MOZ_ASSERT(hops_ < SCOPECOORD_HOPS_LIMIT); return hops_; }
    uint32_t slot() const { MOZ_ASSERT(slot_ < SCOPECOORD_SLOT_LIMIT); return slot_; }

    bool operator==(const ScopeCoordinate& rhs) const {
        return hops() == rhs.hops() && slot() == rhs.slot();
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
        return data().nextFrameSlot;
    }

    Shape* environmentShape() const {
        return data().environmentShape;
    }

    // For frontend use. Implemented in BytecodeEmitter.cpp
    //
    // See if the frame slots for parameters line up exactly between the
    // defaults scope and the body scope. If so, we can omit wasting frame
    // slots and start the body scope at frame slot 0.
    bool optimizeParameterDefaultsFrameSlots(frontend::FunctionBox* funbox);
};

template <>
inline bool
Scope::is<LexicalScope>() const
{
    return kind_ == ScopeKind::Lexical ||
           kind_ == ScopeKind::Catch ||
           kind_ == ScopeKind::ParameterDefaults;
}

class FunctionScope : public Scope
{
    friend class BindingIter;
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

    static FunctionScope* create(ExclusiveContext* cx, Data* data, uint32_t firstFrameSlot,
                                 JSFunction* fun, Scope* enclosing);

  private:
    Data& data() {
        return *reinterpret_cast<Data*>(data_);
    }

    const Data& data() const {
        return *reinterpret_cast<Data*>(data_);
    }

    static uint32_t computeNextFrameSlot(Scope* start) {
        // A function scope's first frame slot may be non-0 when a formal
        // parameter default expression scope is present. In that case, if the
        // defaults scope has any non-closed over bindings, the first function
        // scope frame slot would be non-0.
        if (start->kind() == ScopeKind::ParameterDefaults)
            return start->as<LexicalScope>().nextFrameSlot();
        return 0;
    }

  public:
    uint32_t computeFirstFrameSlot() const {
        return computeNextFrameSlot(enclosing());
    }

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

    uint32_t numSimpleFormalParameters() const {
        return data().nonSimpleFormalStart;
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

  private:
    Data& data() {
        return *reinterpret_cast<Data*>(data_);
    }

    const Data& data() const {
        return *reinterpret_cast<Data*>(data_);
    }

  public:
    bool isNonSyntactic() const {
        return kind() == ScopeKind::NonSyntactic;
    }

    bool hasBindings() const {
        return data().length > 0;
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
        return data().nextFrameSlot;
    }

    Shape* environmentShape() const {
        return data().environmentShape;
    }

    bool strict() const {
        return kind() == ScopeKind::StrictEval;
    }

    bool hasBindings() const {
        return data().length > 0;
    }
};

class BindingIter
{
  protected:
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

    enum CanHaveSlots : uint8_t {
        CannotHaveSlots = 0,
        CanHaveArgumentSlots = 1 << 0,
        CanHaveFrameSlots = 1 << 1,
        CanHaveEnvironmentSlots = 1 << 2
    };

    uint8_t canHaveSlots_;
    uint16_t argumentSlot_;
    uint32_t frameSlot_;
    uint32_t environmentSlot_;

    uint32_t length_;
    BindingName* names_;

    void init(uint16_t nonSimpleFormalStart, uint16_t varStart,
              uint32_t letStart, uint32_t constStart, uint8_t canHaveSlots,
              uint32_t firstFrameSlot, uint32_t firstEnvironmentSlot,
              BindingName* names, uint32_t length)
    {
        nonSimpleFormalStart_ = nonSimpleFormalStart;
        varStart_ = varStart;
        letStart_ = letStart;
        constStart_ = constStart;
        index_ = 0;
        canHaveSlots_ = canHaveSlots;
        argumentSlot_ = 0;
        frameSlot_ = firstFrameSlot;
        environmentSlot_ = firstEnvironmentSlot;
        length_ = length;
        names_ = names;
    }

    void init(LexicalScope::Data& data, uint32_t firstFrameSlot);
    void init(FunctionScope::Data& data, uint32_t firstFrameSlot);
    void init(GlobalScope::Data& data);
    void init(EvalScope::Data& data, bool strict);

  public:
    explicit BindingIter(Scope* scope) {
        switch (scope->kind()) {
          case ScopeKind::ParameterDefaults:
          case ScopeKind::Lexical:
          case ScopeKind::Catch:
            init(scope->as<LexicalScope>().data(),
                 scope->as<LexicalScope>().computeFirstFrameSlot());
            break;
          case ScopeKind::With:
            MOZ_CRASH("With scopes do not have bindings");
            break;
          case ScopeKind::Function:
            init(scope->as<FunctionScope>().data(),
                 scope->as<FunctionScope>().computeFirstFrameSlot());
            break;
          case ScopeKind::Eval:
          case ScopeKind::StrictEval:
            init(scope->as<EvalScope>().data(), scope->kind() == ScopeKind::StrictEval);
            break;
          case ScopeKind::Global:
          case ScopeKind::NonSyntactic:
            init(scope->as<GlobalScope>().data());
            break;
          case ScopeKind::Module:
            MOZ_CRASH("NYI");
        }
    }

    BindingIter(LexicalScope::Data& data, uint32_t firstFrameSlot) {
        init(data, firstFrameSlot);
    }

    explicit BindingIter(FunctionScope::Data& data, uint32_t firstFrameSlot) {
        init(data, firstFrameSlot);
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
        canHaveSlots_(bi.canHaveSlots_),
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
        if (canHaveSlots_) {
            if (index_ < nonSimpleFormalStart_) {
                MOZ_ASSERT(canHaveArgumentSlots());
                argumentSlot_++;
            }
            if (closedOver()) {
                MOZ_ASSERT(canHaveEnvironmentSlots());
                environmentSlot_++;
            } else if (index_ >= nonSimpleFormalStart_) {
                MOZ_ASSERT(canHaveFrameSlots());
                frameSlot_++;
            }
        }
        index_++;
    }

    bool canHaveArgumentSlots() const {
        return canHaveSlots_ & CanHaveArgumentSlots;
    }

    bool canHaveFrameSlots() const {
        return canHaveSlots_ & CanHaveFrameSlots;
    }

    bool canHaveEnvironmentSlots() const {
        return canHaveSlots_ & CanHaveEnvironmentSlots;
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
        if (!canHaveSlots_)
            return BindingLocation::Global();
        if (closedOver()) {
            MOZ_ASSERT(canHaveEnvironmentSlots());
            return BindingLocation::Environment(environmentSlot_);
        }
        if (index_ < nonSimpleFormalStart_) {
            MOZ_ASSERT(canHaveArgumentSlots());
            return BindingLocation::Argument(argumentSlot_);
        }
        MOZ_ASSERT(canHaveFrameSlots());
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

    bool hasArgumentSlot() const {
        MOZ_ASSERT(!done());
        return index_ < nonSimpleFormalStart_;
    }

    uint16_t argumentSlot() const {
        MOZ_ASSERT(canHaveArgumentSlots());
        return mozilla::AssertedCast<uint16_t>(index_);
    }

    uint32_t nextFrameSlot() const {
        MOZ_ASSERT(canHaveFrameSlots());
        return frameSlot_;
    }

    uint32_t nextEnvironmentSlot() const {
        MOZ_ASSERT(canHaveEnvironmentSlots());
        return environmentSlot_;
    }

    void trace(JSTracer* trc);
};

class ClosedOverArgumentSlotIter : public BindingIter
{
    void settle() {
        while (!done() && hasArgumentSlot() && !closedOver())
            BindingIter::operator++(1);
        if (!done() && !hasArgumentSlot())
            index_ = length_;
    }

  public:
    explicit ClosedOverArgumentSlotIter(JSScript* script)
      : BindingIter(script)
    {
        settle();
    }

    void operator++(int) {
        BindingIter::operator++(1);
        settle();
    }
};

class ScopeIter
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

// Starting at scope, count the number of scopes that have syntactic
// environments.
uint32_t EnvironmentChainLength(Scope* scope);

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

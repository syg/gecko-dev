/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef vm_Scope_h
#define vm_Scope_h

#include "mozilla/Maybe.h"
#include "mozilla/Variant.h"

#include "jsobj.h"
#include "jsopcode.h"

#include "gc/Heap.h"
#include "gc/Policy.h"
#include "js/UbiNode.h"
#include "vm/Xdr.h"

namespace js {

class ModuleObject;

enum class BindingKind : uint8_t
{
    Import,
    FormalParameter,
    Var,
    Let,
    Const,

    // So you think named lambda callee names are consts? Nope! They don't
    // throw when being assigned to in sloppy mode.
    NamedLambdaCallee
};

static inline bool
BindingKindIsLexical(BindingKind kind)
{
    return kind == BindingKind::Let || kind == BindingKind::Const;
}

enum class ScopeKind : uint8_t
{
    // FunctionScope
    Function,

    // LexicalScope
    ParameterDefaults,
    Lexical,
    Catch,
    NamedLambda,
    StrictNamedLambda,

    // WithScope
    With,

    // EvalScope
    Eval,
    StrictEval,

    // GlobalScope
    Global,
    NonSyntactic,

    // ModuleScope
    Module
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

    void trace(JSTracer* trc);
};

class BindingLocation
{
  public:
    enum class Kind {
        Global,
        Argument,
        Frame,
        Environment,
        Import,
        NamedLambdaCallee
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
        MOZ_ASSERT(slot < LOCALNO_LIMIT);
        return BindingLocation(Kind::Frame, slot);
    }

    static BindingLocation Environment(uint32_t slot) {
        MOZ_ASSERT(slot < ENVCOORD_SLOT_LIMIT);
        return BindingLocation(Kind::Environment, slot);
    }

    static BindingLocation Import() {
        return BindingLocation(Kind::Import, UINT32_MAX);
    }

    static BindingLocation NamedLambdaCallee() {
        return BindingLocation(Kind::NamedLambdaCallee, UINT32_MAX);
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
        MOZ_ASSERT(kind_ == Kind::Frame || kind_ == Kind::Environment);
        return slot_;
    }

    uint16_t argumentSlot() const {
        MOZ_ASSERT(kind_ == Kind::Argument);
        return mozilla::AssertedCast<uint16_t>(slot_);
    }
};

//
// The base class of all Scopes.
//
class Scope : public js::gc::TenuredCell
{
    friend class GCMarker;

    // The kind determines data_.
    ScopeKind kind_;

    // The enclosing scope or nullptr.
    GCPtrScope enclosing_;

    // If there are any aliased bindings, the shape for the
    // EnvironmentObject. Otherwise nullptr.
    GCPtrShape environmentShape_;

  protected:
    // Most scope data are arrays of JSAtoms, which may be shared across
    // runtimes. FunctionScope is an exception; see comments above
    // FunctionScope::Data.
    struct RefCountedData
    {
        uint32_t refCount;

        void addRef() {
            refCount++;
        }

        void release(FreeOp* fop);
    };

    uintptr_t data_;

    Scope(ScopeKind kind, Scope* enclosing, Shape* environmentShape, uintptr_t data)
      : kind_(kind),
        enclosing_(enclosing),
        environmentShape_(environmentShape),
        data_(data)
    { }

    static Scope* create(ExclusiveContext* cx, ScopeKind kind, HandleScope enclosing,
                         HandleShape envShape, uintptr_t data);

    template <typename ConcreteScope, XDRMode mode>
    static bool XDRSizedBindingData(XDRState<mode>* xdr, Handle<ConcreteScope*> scope,
                                    MutableHandle<typename ConcreteScope::BindingData*> data);

    Shape* maybeCloneEnvironmentShape(JSContext* cx);

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

    Shape* environmentShape() const {
        return environmentShape_;
    }

    bool hasEnvironment() const {
        switch (kind()) {
          case ScopeKind::With:
          case ScopeKind::Global:
          case ScopeKind::NonSyntactic:
            return true;
          default:
            // If there's a shape, an environment must be created for this scope.
            return environmentShape_ != nullptr;
        }
    }

    uint32_t chainLength() const;
    uint32_t environmentChainLength() const;

    template <typename T>
    bool hasOnChain() const {
        for (const Scope* it = this; it; it = it->enclosing()) {
            if (it->is<T>())
                return true;
        }
        return false;
    }

    bool hasOnChain(ScopeKind kind) const {
        for (const Scope* it = this; it; it = it->enclosing()) {
            if (it->kind() == kind)
                return true;
        }
        return false;
    }

    // GlobalScopes and FunctionScopes have extra data that's needed when
    // cloning and cannot use the generic clone.
    static Scope* clone(JSContext* cx, HandleScope scope, HandleScope enclosing);

    void traceChildren(JSTracer* trc);
    void finalize(FreeOp* fop);

    void dump();
};

//
// A lexical scope that holds let and const bindings. There are 3 kinds of
// LexicalScopes.
//
// Lexical
//   A plain lexical scope.
//
// Catch
//   Holds the catch parameters (and only the catch parameters) of a catch
//   block.
//
// ParameterDefaults
//   Holds the parameter names for a function if it has parameter default
//   expressions.
//
// All kinds of LexicalScopes correspond to LexicalEnvironmentObjects on the
// environment chain.
//
class LexicalScope : public Scope
{
    friend class Scope;
    friend class BindingIter;

  public:
    // Data is public because it is created by the frontend. See
    // Parser<FullParseHandler>::newLexicalScopeData.
    struct BindingData : public RefCountedData
    {
        // Bindings are sorted by kind in both frames and environments.
        //
        //   lets - [0, constStart)
        // consts - [constStart, length)
        uint32_t constStart;
        uint32_t length;

        // Frame slots [0, nextFrameSlot) are live when this is the innermost
        // scope.
        uint32_t nextFrameSlot;

        // The array of tagged JSAtom* names, allocated beyond the end of the
        // struct.
        BindingName names[1];

        void trace(JSTracer* trc);
    };

    static size_t sizeOfBindingData(uint32_t length) {
        MOZ_ASSERT(length > 0);
        return sizeof(BindingData) + (length - 1) * sizeof(BindingName);
    }

    static LexicalScope* create(ExclusiveContext* cx, ScopeKind kind, Handle<BindingData*> data,
                                uint32_t firstFrameSlot, HandleScope enclosing);

    template <XDRMode mode>
    static bool XDR(XDRState<mode>* xdr, ScopeKind kind, HandleScope enclosing,
                    MutableHandleScope scope);

  protected:
    BindingData& bindingData() {
        return *reinterpret_cast<BindingData*>(data_);
    }

    const BindingData& bindingData() const {
        return *reinterpret_cast<BindingData*>(data_);
    }

    static uint32_t nextFrameSlot(Scope* start);

  public:
    uint32_t firstFrameSlot() const;

    uint32_t nextFrameSlot() const {
        return bindingData().nextFrameSlot;
    }

    size_t sizeOfData(mozilla::MallocSizeOf mallocSizeOf) const;

    // Returns an empty shape for extensible global and non-syntactic lexical
    // scopes.
    static Shape* getEmptyExtensibleEnvironmentShape(ExclusiveContext* cx);
};

template <>
inline bool
Scope::is<LexicalScope>() const
{
    return kind_ == ScopeKind::Lexical ||
           kind_ == ScopeKind::Catch ||
           kind_ == ScopeKind::ParameterDefaults ||
           kind_ == ScopeKind::NamedLambda ||
           kind_ == ScopeKind::StrictNamedLambda;
}

//
// Scope corresponding to a function body. Holds var bindings and formal
// parameter names.
//
// Corresponds to CallObject on environment chain.
//
class FunctionScope : public Scope
{
    friend class GCMarker;
    friend class BindingIter;
    friend class PositionalFormalParameterIter;
    friend class Scope;
    static const ScopeKind classScopeKind_ = ScopeKind::Function;

  public:
    // BindingData is public because it is created by the
    // frontend. See Parser<FullParseHandler>::newFunctionScopeData.
    struct BindingData : public RefCountedData
    {
        // Bindings are sorted by kind in both frames and environments.
        //
        // Positional formal parameter names are those without default
        // expressions or destructuring, i.e. those that may be referred to by
        // argument slots.
        //
        // An argument slot that needs to be skipped due to being destructured
        // or having defaults will have a nullptr name in the name array to
        // advance the argument slot.
        //
        // positional formals - [0, nonPositionalFormalStart)
        //      other formals - [nonPositionalParamStart, varStart)
        //               vars - [varStart, length)
        uint16_t nonPositionalFormalStart;
        uint16_t varStart;
        uint32_t length;

        // Frame slots [0, nextFrameSlot) are live when this is the innermost
        // scope.
        uint32_t nextFrameSlot;

        // The array of tagged JSAtom* names, allocated beyond the end of the
        // struct.
        BindingName names[1];

        void trace(JSTracer* trc);
    };

    // Because canonicalFunction is per-compartment and cannot be shared,
    // FunctionScopes have two data pointers, one for shareable data and one
    // for per-compartment data.
    struct Data
    {
        // The canonical function of the scope, as during a scope walk we
        // often query properties of the JSFunction (e.g., is the function an
        // arrow).
        GCPtrFunction canonicalFunction;

        // Shareable bindings.
        BindingData* bindings;

        void trace(JSTracer* trc);
    };

    static size_t sizeOfBindingData(uint32_t length) {
        MOZ_ASSERT(length > 0);
        return sizeof(BindingData) + (length - 1) * sizeof(BindingName);
    }

    static FunctionScope* create(ExclusiveContext* cx, Handle<BindingData*> data,
                                 uint32_t firstFrameSlot, bool hasDefaults, bool needsEnvironment,
                                 HandleFunction fun, HandleScope enclosing);

    template <XDRMode mode>
    static bool XDR(XDRState<mode>* xdr, HandleFunction fun, HandleScope enclosing,
                    MutableHandleScope scope);

  private:
    Data& data() {
        return *reinterpret_cast<Data*>(data_);
    }

    const Data& data() const {
        return *reinterpret_cast<Data*>(data_);
    }

    BindingData& bindingData() {
        return *data().bindings;
    }

     const BindingData& bindingData() const {
        return *data().bindings;
    }

    static uint32_t nextFrameSlot(Scope* scope) {
        // A function scope's first frame slot may be non-0 when a formal
        // parameter default expression scope is present. In that case, if the
        // defaults scope has any non-closed over bindings, the first function
        // scope frame slot would be non-0.
        if (scope->kind() == ScopeKind::ParameterDefaults)
            return scope->as<LexicalScope>().nextFrameSlot();
        return 0;
    }

  public:
    static FunctionScope* clone(JSContext* cx, Handle<FunctionScope*> scope, HandleFunction fun,
                                HandleScope enclosing);

    uint32_t firstFrameSlot() const {
        return nextFrameSlot(enclosing());
    }

    uint32_t nextFrameSlot() const {
        return bindingData().nextFrameSlot;
    }

    JSFunction* canonicalFunction() const {
        return data().canonicalFunction;
    }

    JSScript* script() const;

    uint32_t numPositionalFormalParameters() const {
        return bindingData().nonPositionalFormalStart;
    }

    size_t sizeOfData(mozilla::MallocSizeOf mallocSizeOf) const;

    static Shape* getEmptyEnvironmentShape(ExclusiveContext* cx);
};

//
// Scope corresponding to both the global object scope and the global lexical
// scope.
//
// Both are extensible and are singletons across <script> tags, so these
// scopes are a fragment of the names in global scope. In other words, two
// global scripts may have two different GlobalScopes despite having the same
// GlobalObject.
//
// There are 2 kinds of GlobalScopes.
//
// Global
//   Corresponds to a GlobalObject and its global LexicalEnvironmentObject on
//   the environment chain.
//
// NonSyntactic
//   Corresponds to a non-GlobalObject created by the embedding on the
//   environment chain. This distinction is important for optimizations.
//
class GlobalScope : public Scope
{
    friend class Scope;
    friend class BindingIter;

  public:
    // Data is public because it is created by the frontend. See
    // Parser<FullParseHandler>::newGlobalScopeData.
    struct BindingData : public RefCountedData
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

        void trace(JSTracer* trc);
    };

    static size_t sizeOfBindingData(uint32_t length) {
        MOZ_ASSERT(length > 0);
        return sizeof(BindingData) + (length - 1) * sizeof(BindingName);
    }

    static GlobalScope* create(ExclusiveContext* cx, ScopeKind kind, Handle<BindingData*> data);

    static GlobalScope* createEmpty(ExclusiveContext* cx, ScopeKind kind) {
        return create(cx, kind, nullptr);
    }

    template <XDRMode mode>
    static bool XDR(XDRState<mode>* xdr, ScopeKind kind, MutableHandleScope scope);

  private:
    BindingData& bindingData() {
        return *reinterpret_cast<BindingData*>(data_);
    }

    const BindingData& bindingData() const {
        return *reinterpret_cast<BindingData*>(data_);
    }

  public:
    static GlobalScope* clone(JSContext* cx, Handle<GlobalScope*> scope, ScopeKind kind);

    bool isSyntactic() const {
        return kind() != ScopeKind::NonSyntactic;
    }

    bool hasBindings() const {
        return bindingData().length > 0;
    }

    size_t sizeOfData(mozilla::MallocSizeOf mallocSizeOf) const;
};

template <>
inline bool
Scope::is<GlobalScope>() const
{
    return kind_ == ScopeKind::Global || kind_ == ScopeKind::NonSyntactic;
}

//
// Scope of a 'with' statement. Has no bindings.
//
// Corresponds to a WithEnvironmentObject on the environment chain.
class WithScope : public Scope
{
    friend class Scope;
    static const ScopeKind classScopeKind_ = ScopeKind::With;

  public:
    static WithScope* create(ExclusiveContext* cx, HandleScope enclosing);
};

//
// Scope of an eval. Holds var bindings. There are 2 kinds of EvalScopes.
//
// ScopeKind::StrictEval
//   A strict eval. Corresponds to a CallObject, where its var bindings lives.
//
// ScopeKind::Eval
//   A sloppy eval. If this is a direct `eval()` call inside a parameter
//   default value expression, then this is like a StrictEval scope (per
//   spec). Anywhere else, this is an empty scope, used only in the frontend,
//   to detect redeclaration errors. It has no Environment. Any `var`s declared
//   in the eval code are bound on the nearest enclosing var environment.
//
class EvalScope : public Scope
{
    friend class Scope;
    friend class BindingIter;

  public:
    // BindingData is public because it is created by the frontend. See
    // Parser<FullParseHandler>::newEvalScopeData.
    struct BindingData : public RefCountedData
    {
        // All bindings in an eval script are 'var' bindings. The implicit
        // lexical scope around the eval is present regardless of strictness
        // and is its own LexicalScope.
        uint32_t length;

        // Frame slots [0, nextFrameSlot) are live when this is the innermost
        // scope.
        uint32_t nextFrameSlot;

        // The array of tagged JSAtom* names, allocated beyond the end of the
        // struct.
        BindingName names[1];

        void trace(JSTracer* trc);
    };

    static size_t sizeOfBindingData(uint32_t length) {
        MOZ_ASSERT(length > 0);
        return sizeof(BindingData) + (length - 1) * sizeof(BindingName);
    }

    static EvalScope* create(ExclusiveContext* cx, ScopeKind kind, Handle<BindingData*> data,
                             HandleScope enclosing);

    template <XDRMode mode>
    static bool XDR(XDRState<mode>* xdr, ScopeKind kind, HandleScope enclosing,
                    MutableHandleScope scope);

  private:
    BindingData& bindingData() {
        return *reinterpret_cast<BindingData*>(data_);
    }

    const BindingData& bindingData() const {
        return *reinterpret_cast<BindingData*>(data_);
    }

  public:
    // Starting a scope, the nearest var scope that a direct eval can
    // introduce vars on.
    static Scope* nearestVarScopeForDirectEval(Scope* scope);

    uint32_t nextFrameSlot() const {
        return bindingData().nextFrameSlot;
    }

    bool strict() const {
        return kind() == ScopeKind::StrictEval;
    }

    bool hasBindings() const {
        return bindingData().length > 0;
    }

    bool isNonGlobal() const {
        if (strict())
            return true;
        return !nearestVarScopeForDirectEval(enclosing())->is<GlobalScope>();
    }

    size_t sizeOfData(mozilla::MallocSizeOf mallocSizeOf) const;

    static Shape* getEmptyEnvironmentShape(ExclusiveContext* cx);
};

template <>
inline bool
Scope::is<EvalScope>() const
{
    return kind_ == ScopeKind::Eval || kind_ == ScopeKind::StrictEval;
}

//
// Scope corresponding to the toplevel script in an ES module.
//
// Like GlobalScopes, these scopes contain both vars and lexical bindings, as
// the treating of imports and exports requires putting them in one scope.
//
// Corresponds to a ModuleEnvironmentObject on the environment chain.
//
class ModuleScope : public Scope
{
    friend class GCMarker;
    friend class BindingIter;
    friend class Scope;
    static const ScopeKind classScopeKind_ = ScopeKind::Module;

  public:
    // Data is public because it is created by the frontend. See
    // Parser<FullParseHandler>::newModuleScopeData.
    struct BindingData : public RefCountedData
    {
        // Bindings are sorted by kind.
        //
        // imports - [0, varStart)
        //    vars - [varStart, letStart)
        //    lets - [letStart, constStart)
        //  consts - [constStart, length)
        uint32_t varStart;
        uint32_t letStart;
        uint32_t constStart;
        uint32_t length;

        // Frame slots [0, nextFrameSlot) are live when this is the innermost
        // scope.
        uint32_t nextFrameSlot;

        // Frame slots [0, varFrameSlotEnd) are vars.
        uint32_t varFrameSlotEnd;

        // The array of tagged JSAtom* names, allocated beyond the end of the
        // struct.
        BindingName names[1];

        void trace(JSTracer* trc);
    };

    // Because module is per-compartment and cannot be shared, ModuleScopes
    // have two data pointers, one for shareable data and one for
    // per-compartment data.
    struct Data
    {
        // The module of the scope.
        GCPtr<ModuleObject*> module;

        // Shareable bindings.
        BindingData* bindings;

        void trace(JSTracer* trc);
    };

    static size_t sizeOfBindingData(uint32_t length) {
        MOZ_ASSERT(length > 0);
        return sizeof(BindingData) + (length - 1) * sizeof(BindingName);
    }

    static ModuleScope* create(ExclusiveContext* cx, Handle<BindingData*> bindings,
                               Handle<ModuleObject*> module, HandleScope enclosing);

  private:
    Data& data() {
        return *reinterpret_cast<Data*>(data_);
    }

    const Data& data() const {
        return *reinterpret_cast<Data*>(data_);
    }

    BindingData& bindingData() {
        return *data().bindings;
    }

    const BindingData& bindingData() const {
        return *data().bindings;
    }

  public:
    uint32_t nextFrameSlot() const {
        return bindingData().nextFrameSlot;
    }

    uint32_t varFrameSlotEnd() const {
        return bindingData().varFrameSlotEnd;
    }

    ModuleObject* module() const {
        return data().module;
    }

    JSScript* script() const;

    size_t sizeOfData(mozilla::MallocSizeOf mallocSizeOf) const;

    static Shape* getEmptyEnvironmentShape(ExclusiveContext* cx);
};

//
// An iterator for a Scope's bindings. This is the source of truth for frame
// and environment object layout.
//
// It may be placed in GC containers; for example:
//
//   for (Rooted<BindingIter> bi(cx, BindingIter(scope)); bi; bi++) {
//     use(bi);
//     SomeMayGCOperation();
//     use(bi);
//   }
//
class BindingIter
{
  protected:
    // Bindings are sorted by kind. Because different Scopes have differently
    // laid out BindingData for packing, BindingIter must handle all binding kinds.
    //
    // Kind ranges:
    //
    //            imports - [0, positionalFormalStart)
    // positional formals - [positionalFormalStart, nonPositionalFormalStart)
    //      other formals - [nonPositionalParamStart, varStart)
    //               vars - [varStart, letStart)
    //               lets - [letStart, constStart)
    //             consts - [constStart, length)
    //
    // Access method when not closed over:
    //
    //            imports - name
    // positional formals - argument slot
    //      other formals - frame slot
    //               vars - frame slot
    //               lets - frame slot
    //             consts - frame slot
    //
    // Access method when closed over:
    //
    //            imports - name
    // positional formals - environment slot
    //      other formals - environment slot
    //               vars - environment slot
    //               lets - environment slot
    //             consts - environment slot
    uint32_t positionalFormalStart_;
    uint32_t nonPositionalFormalStart_;
    uint32_t varStart_;
    uint32_t letStart_;
    uint32_t constStart_;
    uint32_t length_;

    uint32_t index_;

    enum Flags : uint8_t {
        CannotHaveSlots = 0,
        CanHaveArgumentSlots = 1 << 0,
        CanHaveFrameSlots = 1 << 1,
        CanHaveEnvironmentSlots = 1 << 2,

        // See comment in settle below.
        IgnorePositionalFormalParameters = 1 << 3,
        IgnoreDestructuredFormalParameters = 1 << 4,

        // Truly I hate named lambdas.
        IsNamedLambda = 1 << 5
    };

    static const uint8_t CanHaveSlotsMask = 0x7;

    uint8_t flags_;
    uint16_t argumentSlot_;
    uint32_t frameSlot_;
    uint32_t environmentSlot_;

    BindingName* names_;

    void init(uint32_t positionalFormalStart, uint32_t nonPositionalFormalStart,
              uint32_t varStart, uint32_t letStart, uint32_t constStart,
              uint8_t flags, uint32_t firstFrameSlot, uint32_t firstEnvironmentSlot,
              BindingName* names, uint32_t length)
    {
        positionalFormalStart_ = positionalFormalStart;
        nonPositionalFormalStart_ = nonPositionalFormalStart;
        varStart_ = varStart;
        letStart_ = letStart;
        constStart_ = constStart;
        length_ = length;
        index_ = 0;
        flags_ = flags;
        argumentSlot_ = 0;
        frameSlot_ = firstFrameSlot;
        environmentSlot_ = firstEnvironmentSlot;
        names_ = names;

        settle();
    }

    void init(LexicalScope::BindingData& data, uint32_t firstFrameSlot, uint8_t flags);
    void init(FunctionScope::BindingData& data, uint32_t firstFrameSlot, uint8_t flags);
    void init(GlobalScope::BindingData& data);
    void init(EvalScope::BindingData& data, bool strict);
    void init(ModuleScope::BindingData& data);

    bool ignorePositionalFormalParameters() const {
        return flags_ & IgnorePositionalFormalParameters;
    }

    bool ignoreDestructuredFormalParameters() const {
        return flags_ & IgnoreDestructuredFormalParameters;
    }

    bool isNamedLambda() const {
        return flags_ & IsNamedLambda;
    }

    void increment() {
        MOZ_ASSERT(!done());
        if (flags_ & CanHaveSlotsMask) {
            if (canHaveArgumentSlots()) {
                if (index_ < nonPositionalFormalStart_) {
                    MOZ_ASSERT(index_ >= positionalFormalStart_);
                    argumentSlot_++;
                }
            }
            if (closedOver()) {
                // Imports must not be given known slots. They are
                // indirect bindings.
                MOZ_ASSERT(kind() != BindingKind::Import);
                MOZ_ASSERT(canHaveEnvironmentSlots());
                environmentSlot_++;
            } else if (canHaveFrameSlots()) {
                if (index_ >= nonPositionalFormalStart_)
                    frameSlot_++;
            }
        }
        index_++;
    }

    void settle() {
        // Special settling behavior is only needed for formal parameters.
        if (nonPositionalFormalStart_ == 0)
            return;

        // If a FunctionScope has parameters with default expressions, the
        // parameters act like lexical bindings in the parameter defaults
        // scope, which encloses the function scope. That is, they don't
        // participate in the frame/environment layout of the FunctionScope.
        //
        // However, for error reporting and decompiling argument names, we
        // still need to keep around the binding names. When iterating
        // bindings for the purpose of computing frame/environment layout,
        // ignore formal parameter names in FunctionScopes with parameters
        // that have default expressions.
        if (ignorePositionalFormalParameters()) {
            // There can't be imports in a function scope.
            MOZ_ASSERT(positionalFormalStart_ == 0);
            while (index_ < nonPositionalFormalStart_)
                increment();
        } else if (ignoreDestructuredFormalParameters()) {
            while (!done() && !name())
                increment();
        }
    }

  public:
    explicit BindingIter(Scope* scope);
    explicit BindingIter(JSScript* script);

    BindingIter(LexicalScope::BindingData& data, uint32_t firstFrameSlot, bool isNamedLambda) {
        init(data, firstFrameSlot, isNamedLambda ? IsNamedLambda : 0);
    }

    BindingIter(FunctionScope::BindingData& data, uint32_t firstFrameSlot, bool hasDefaults) {
        init(data, firstFrameSlot,
             IgnoreDestructuredFormalParameters |
             (hasDefaults ? IgnorePositionalFormalParameters : 0));
    }

    explicit BindingIter(GlobalScope::BindingData& data) {
        init(data);
    }

    explicit BindingIter(ModuleScope::BindingData& data) {
        init(data);
    }

    BindingIter(EvalScope::BindingData& data, bool strict) {
        init(data, strict);
    }

    explicit BindingIter(const BindingIter& bi)
      : positionalFormalStart_(bi.positionalFormalStart_),
        nonPositionalFormalStart_(bi.nonPositionalFormalStart_),
        varStart_(bi.varStart_),
        letStart_(bi.letStart_),
        constStart_(bi.constStart_),
        length_(bi.length_),
        index_(bi.index_),
        flags_(bi.flags_),
        argumentSlot_(bi.argumentSlot_),
        frameSlot_(bi.frameSlot_),
        environmentSlot_(bi.environmentSlot_),
        names_(bi.names_)
    { }

    bool done() const {
        return index_ == length_;
    }

    explicit operator bool() const {
        return !done();
    }

    void operator++(int) {
        increment();
        settle();
    }

    bool isLast() const {
        MOZ_ASSERT(!done());
        return index_ + 1 == length_;
    }

    bool canHaveArgumentSlots() const {
        return flags_ & CanHaveArgumentSlots;
    }

    bool canHaveFrameSlots() const {
        return flags_ & CanHaveFrameSlots;
    }

    bool canHaveEnvironmentSlots() const {
        return flags_ & CanHaveEnvironmentSlots;
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
        if (!(flags_ & CanHaveSlotsMask))
            return BindingLocation::Global();
        if (index_ < positionalFormalStart_)
            return BindingLocation::Import();
        if (closedOver()) {
            MOZ_ASSERT(canHaveEnvironmentSlots());
            return BindingLocation::Environment(environmentSlot_);
        }
        if (index_ < nonPositionalFormalStart_) {
            MOZ_ASSERT(canHaveArgumentSlots());
            return BindingLocation::Argument(argumentSlot_);
        }
        if (canHaveFrameSlots())
            return BindingLocation::Frame(frameSlot_);
        MOZ_ASSERT(isNamedLambda());
        return BindingLocation::NamedLambdaCallee();
    }

    BindingKind kind() const {
        MOZ_ASSERT(!done());
        if (index_ < positionalFormalStart_)
            return BindingKind::Import;
        if (index_ < varStart_)
            return BindingKind::FormalParameter;
        if (index_ < letStart_)
            return BindingKind::Var;
        if (index_ < constStart_)
            return BindingKind::Let;
        if (isNamedLambda())
            return BindingKind::NamedLambdaCallee;
        return BindingKind::Const;
    }

    bool hasArgumentSlot() const {
        MOZ_ASSERT(!done());
        return index_ < nonPositionalFormalStart_;
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

void DumpBindings(JSContext* cx, Scope* scope);

//
// A refinement BindingIter that only iterates over positional formal
// parameters of a function.
//
class PositionalFormalParameterIter : public BindingIter
{
    bool hasDefaults_;

    void settle() {
        if (index_ >= nonPositionalFormalStart_)
            index_ = length_;
    }

  public:
    explicit PositionalFormalParameterIter(JSScript* script);

    void operator++(int) {
        BindingIter::operator++(1);
        settle();
    }

    bool isDestructured() const {
        return !name();
    }

    BindingLocation location() const {
        // The locations reported by this iter are wrong when the script has a
        // defaults scope.
        MOZ_ASSERT(!hasDefaults_);
        return BindingIter::location();
    }
};

//
// Iterator for walking the scope chain.
//
// It may be placed in GC containers; for example:
//
//   for (Rooted<ScopeIter> si(cx, ScopeIter(scope)); si; si++) {
//     use(si);
//     SomeMayGCOperation();
//     use(si);
//   }
//
class ScopeIter
{
    HeapPtr<Scope*> scope_;

  public:
    explicit ScopeIter(Scope* scope)
      : scope_(scope)
    { }

    explicit ScopeIter(JSScript* script);

    explicit ScopeIter(const ScopeIter& si)
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

    // Returns the shape of the environment if it is known. It is possible to
    // hasSyntacticEnvironment and to have no known shape, e.g., eval.
    Shape* environmentShape() const {
        return scope()->environmentShape();
    }

    // Returns whether this scope has a syntactic environment (i.e., an
    // Environment that isn't a non-syntactic With or NonSyntacticVariables)
    // on the environment chain.
    bool hasSyntacticEnvironment() const;

    void trace(JSTracer* trc) {
        TraceNullableEdge(trc, &scope_, "scope iter scope");
    }
};

//
// Specializations of Rooted containers for the iterators.
//

template <typename Outer>
class BindingIterOperations
{
    const BindingIter& iter() const { return static_cast<const Outer*>(this)->get(); }

  public:
    bool done() const { return iter().done(); }
    explicit operator bool() const { return !done(); }
    bool isLast() const { return iter().isLast(); }
    bool canHaveArgumentSlots() const { return iter().canHaveArgumentSlots(); }
    bool canHaveFrameSlots() const { return iter().canHaveFrameSlots(); }
    bool canHaveEnvironmentSlots() const { return iter().canHaveEnvironmentSlots(); }
    JSAtom* name() const { return iter().name(); }
    bool closedOver() const { return iter().closedOver(); }
    BindingLocation location() const { return iter().location(); }
    BindingKind kind() const { return iter().kind(); }
    bool hasArgumentSlot() const { return iter().hasArgumentSlot(); }
    uint16_t argumentSlot() const { return iter().argumentSlot(); }
    uint32_t nextFrameSlot() const { return iter().nextFrameSlot(); }
    uint32_t nextEnvironmentSlot() const { return iter().nextEnvironmentSlot(); }
};

template <typename Outer>
class MutableBindingIterOperations : public BindingIterOperations<Outer>
{
    const BindingIter& iter() const { return static_cast<const Outer*>(this)->get(); }
    BindingIter& iter() { return static_cast<Outer*>(this)->get(); }

  public:
    void operator++(int) { iter().operator++(1); }
};

template <typename Outer>
class ScopeIterOperations
{
    const ScopeIter& iter() const { return static_cast<const Outer*>(this)->get(); }

  public:
    bool done() const { return iter().done(); }
    explicit operator bool() const { return !done(); }
    Scope* scope() const { return iter().scope(); }
    ScopeKind kind() const { return iter().kind(); }
    Shape* environmentShape() const { return iter().environmentShape(); }
    bool hasSyntacticEnvironment() const { return iter().hasSyntacticEnvironment(); }
};

template <typename Outer>
class MutableScopeIterOperations : public ScopeIterOperations<Outer>
{
    const ScopeIter& iter() const { return static_cast<const Outer*>(this)->get(); }
    ScopeIter& iter() { return static_cast<Outer*>(this)->get(); }

  public:
    void operator++(int) { iter().operator++(1); }
};

#define SPECIALIZE_ROOTING_CONTAINERS(Iter)                             \
    template <>                                                         \
    class RootedBase<Iter>                                              \
      : public Mutable##Iter##Operations<JS::Rooted<Iter>>              \
    { };                                                                \
                                                                        \
    template <>                                                         \
    class MutableHandleBase<Iter>                                       \
      : public Mutable##Iter##Operations<JS::MutableHandle<Iter>>       \
    { };                                                                \
                                                                        \
    template <>                                                         \
    class HandleBase<Iter>                                              \
      : public Iter##Operations<JS::Handle<Iter>>                       \
    { };                                                                \
                                                                        \
    template <>                                                         \
    class PersistentRootedBase<Iter>                                    \
      : public Iter##Operations<JS::PersistentRooted<Iter>>             \
    { }

SPECIALIZE_ROOTING_CONTAINERS(BindingIter);
SPECIALIZE_ROOTING_CONTAINERS(ScopeIter);

#undef SPECIALIZE_ROOTING_CONTAINERS

//
// Allow using is<T> and as<T> on Rooted<Scope*> and Handle<Scope*>.
//

template <typename Outer>
struct ScopeCastOperation
{
    template <class U>
    JS::Handle<U*> as() const {
        const Outer& self = *static_cast<const Outer*>(this);
        MOZ_ASSERT_IF(self, self->template is<U>());
        return Handle<U*>::fromMarkedLocation(reinterpret_cast<U* const*>(self.address()));
    }
};

template <>
class RootedBase<Scope*> : public ScopeCastOperation<JS::Rooted<Scope*>>
{ };

template <>
class HandleBase<Scope*> : public ScopeCastOperation<JS::Handle<Scope*>>
{ };

template <>
class MutableHandleBase<Scope*> : public ScopeCastOperation<JS::MutableHandle<Scope*>>
{ };

} // namespace js

namespace JS {

template <>
struct GCPolicy<js::ScopeKind> : public IgnoreGCPolicy<js::ScopeKind>
{ };

template <typename T>
struct ScopeDataGCPolicy
{
    static T initial() {
        return nullptr;
    }

    static void trace(JSTracer* trc, T* vp, const char* name) {
        if (*vp)
            (*vp)->trace(trc);
    }
};

#define DEFINE_SCOPE_DATA_GCPOLICY(Data)                        \
    template <>                                                 \
    struct MapTypeToRootKind<Data*> {                           \
        static const RootKind kind = RootKind::Traceable;       \
    };                                                          \
    template <>                                                 \
    struct GCPolicy<Data*> : public ScopeDataGCPolicy<Data*>    \
    { }

DEFINE_SCOPE_DATA_GCPOLICY(js::LexicalScope::BindingData);
DEFINE_SCOPE_DATA_GCPOLICY(js::FunctionScope::BindingData);
DEFINE_SCOPE_DATA_GCPOLICY(js::FunctionScope::Data);
DEFINE_SCOPE_DATA_GCPOLICY(js::GlobalScope::BindingData);
DEFINE_SCOPE_DATA_GCPOLICY(js::EvalScope::BindingData);
DEFINE_SCOPE_DATA_GCPOLICY(js::ModuleScope::BindingData);
DEFINE_SCOPE_DATA_GCPOLICY(js::ModuleScope::Data);

#undef DEFINE_SCOPE_DATA_GCPOLICY

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

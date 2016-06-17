/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef vm_ScopeObject_h
#define vm_ScopeObject_h

#include "jscntxt.h"
#include "jsobj.h"
#include "jsweakmap.h"

#include "builtin/ModuleObject.h"
#include "gc/Barrier.h"
#include "js/GCHashTable.h"
#include "vm/ArgumentsObject.h"
#include "vm/ProxyObject.h"

namespace js {

class ModuleObject;
typedef Handle<ModuleObject*> HandleModuleObject;


/*
 * Non-syntactic scopes
 *
 * A non-syntactic scope is one that was not created due to source code. On
 * the static scope chain, a single StaticNonSyntacticScope maps to 0+
 * non-syntactic dynamic scope objects. This is contrasted with syntactic
 * scopes, where each syntactic static scope corresponds to 0 or 1 dynamic
 * scope objects.
 *
 * There are 3 kinds of dynamic non-syntactic scopes:
 *
 * 1. DynamicWithObject
 *
 *    When the embedding compiles or executes a script, it has the option to
 *    pass in a vector of objects to be used as the initial scope chain. Each
 *    of those objects is wrapped by a DynamicWithObject.
 *
 *    The innermost scope passed in by the embedding becomes a qualified
 *    variables object that captures 'var' bindings. That is, it wraps the
 *    holder object of 'var' bindings.
 *
 *    Does not hold 'let' or 'const' bindings.
 *
 * 2. NonSyntacticVariablesObject
 *
 *    When the embedding wants qualified 'var' bindings and unqualified
 *    bareword assignments to go on a different object than the global
 *    object. While any object can be made into a qualified variables object,
 *    only the GlobalObject and NonSyntacticVariablesObject are considered
 *    unqualified variables objects.
 *
 *    Unlike DynamicWithObjects, this object is itself the holder of 'var'
 *    bindings.
 *
 *    Does not hold 'let' or 'const' bindings.
 *
 * 3. ClonedBlockObject
 *
 *    Each non-syntactic object used as a qualified variables object needs to
 *    enclose a non-syntactic ClonedBlockObject to hold 'let' and 'const'
 *    bindings. There is a bijection per compartment between the non-syntactic
 *    variables objects and their non-syntactic ClonedBlockObjects.
 *
 *    Does not hold 'var' bindings.
 *
 * The embedding (Gecko) uses non-syntactic scopes for various things, some of
 * which are detailed below. All scope chain listings below are, from top to
 * bottom, outermost to innermost.
 *
 * A. Component loading
 *
 * Components may be loaded in "reuse loader global" mode, where to save on
 * memory, all JSMs and JS-implemented XPCOM modules are loaded into a single
 * global. Each individual JSMs are compiled as functions with their own
 * FakeBackstagePass. They have the following dynamic scope chain:
 *
 *   BackstagePass global
 *       |
 *   Global lexical scope
 *       |
 *   DynamicWithObject wrapping FakeBackstagePass
 *       |
 *   ClonedBlockObject
 *
 * B. Subscript loading
 *
 * Subscripts may be loaded into a target object. They have the following
 * dynamic scope chain:
 *
 *   Loader global
 *       |
 *   Global lexical scope
 *       |
 *   DynamicWithObject wrapping target
 *       |
 *   ClonedBlockObject
 *
 * C. Frame scripts
 *
 * XUL frame scripts are always loaded with a NonSyntacticVariablesObject as a
 * "polluting global". This is done exclusively in
 * js::ExecuteInGlobalAndReturnScope.
 *
 *   Loader global
 *       |
 *   Global lexical scope
 *       |
 *   NonSyntacticVariablesObject
 *       |
 *   ClonedBlockObject
 *
 * D. XBL
 *
 * XBL methods are compiled as functions with XUL elements on the scope chain.
 * For a chain of elements e0,...,eN:
 *
 *      ...
 *       |
 *   DynamicWithObject wrapping eN
 *       |
 *      ...
 *       |
 *   DynamicWithObject wrapping e0
 *       |
 *   ClonedBlockObject
 *
 */


/*****************************************************************************/

/*
 * Return a shape representing the static scope containing the variable
 * accessed by the ALIASEDVAR op at 'pc'.
 */
extern Shape*
ScopeCoordinateToEnvironmentShape(JSScript* script, jsbytecode* pc);

/* Return the name being accessed by the given ALIASEDVAR op. */
extern PropertyName*
ScopeCoordinateName(ScopeCoordinateNameCache& cache, JSScript* script, jsbytecode* pc);

/* Return the function script accessed by the given ALIASEDVAR op, or nullptr. */
extern JSScript*
ScopeCoordinateFunctionScript(JSScript* script, jsbytecode* pc);


/*** Scope objects *******************************************************************************/

/*
 * Scope objects are technically real JSObjects but only belong on the scope
 * chain (that is, fp->scopeChain() or fun->environment()). The hierarchy of
 * scope objects is:
 *
 *   JSObject                       Generic object
 *     |   |
 *     |  StaticScope               Created at compile time
 *     |   |   |   |
 *     |   |   |  StaticNonSyntacticScope   See "Non-syntactic scopes"
 *     |   |   |
 *     |   |  StaticEvalScope       Placeholder so eval scopes may be iterated through
 *     |   |
 *     |  NestedStaticScope         Enclosing scope is in the same JSScript
 *     |   |   |
 *     |   |  StaticBlockScope      See NB
 *     |   |
 *     |  StaticWithScope           Template for "with" object in static scope chain
 *     |
 *   ScopeObject                    Engine-internal scope
 *     |   |   |
 *     |   |  DeclEnvObject         Holds name of recursive/needsCallObject named lambda
 *     |   |
 *     |  LexicalScopeBase          Shared base for function and modules scopes
 *     |   |   |
 *     |   |  CallObject            Scope of entire function or strict eval
 *     |   |
 *     |  ModuleEnvironmentObject   Module top-level scope on run-time scope chain
 *     |
 *   NestedScopeObject              Statement scopes; don't cross script boundaries
 *     |   |
 *     |  DynamicWithObject         Run-time "with" object on scope chain
 *     |
 *   ClonedBlockObject              let, switch, catch, for
 *
 * This hierarchy represents more than just the interface hierarchy: reserved
 * slots in base classes are fixed for all derived classes. Thus, for example,
 * ScopeObject::enclosingScope() can simply access a fixed slot without further
 * dynamic type information.
 *
 * NB: Static block objects are a special case: these objects are created at
 * compile time to hold the shape/binding information from which block objects
 * are cloned at runtime. These objects should never escape into the wild and
 * support a restricted set of ScopeObject operations.
 *
 * See also "Debug scope objects" below.
 */

class EnvironmentObject : public NativeObject
{
  protected:
    // The enclosing environment. Either another EnvironmentObject, a
    // GlobalObject, or a non-syntactic environment object.
    static const uint32_t ENCLOSING_ENV_SLOT = 0;

    inline void setAliasedBinding(JSContext* cx, uint32_t slot, PropertyName* name,
                               const Value& v);

  public:
    // Since every env chain terminates with a global object, whether
    // GlobalObject or a non-syntactic one, and since those objects do not
    // derive EnvironmentObject (they have completely different layouts), the
    // enclosing environment of an EnvironmentObject is necessarily non-null.
    JSObject& enclosingEnvironment() const {
        return getReservedSlot(ENCLOSING_ENV_SLOT).toObject();
    }

    void initEnclosingEnvironment(JSObject* enclosing) {
        initReservedSlot(ENCLOSING_ENV_SLOT, ObjectOrNullValue(enclosing));
    }

    // Get or set a name contained in this environment.
    const Value& aliasedBinding(ScopeCoordinate sc) {
        return getSlot(sc.slot());
    }

    const Value& aliasedBinding(const BindingIter& bi) {
        MOZ_ASSERT(bi.location().kind() == BindingLocation::Kind::Environment);
        return getSlot(bi.location().slot());
    }

    inline void setAliasedBinding(JSContext* cx, ScopeCoordinate sc, PropertyName* name,
                                  const Value& v);

    inline void setAliasedBinding(JSContext* cx, const BindingIter& bi, const Value& v);

    // For JITs.
    static size_t offsetOfEnclosingEnvironment() {
        return getFixedSlotOffset(ENCLOSING_ENV_SLOT);
    }

    static uint32_t enclosingEnvironmentSlot() {
        return ENCLOSING_ENV_SLOT;
    }
};

class VarEnvironmentObject : public EnvironmentObject { };

class CallObject : public VarEnvironmentObject
{
  protected:
    static const uint32_t CALLEE_SLOT = 1;

    static CallObject* create(JSContext* cx, HandleScript script, HandleObject enclosing,
                              HandleFunction callee);

  public:
    static const uint32_t RESERVED_SLOTS = 2;
    static const Class class_;

    /* These functions are internal and are exposed only for JITs. */

    /*
     * Construct a bare-bones call object given a shape and a non-singleton
     * group.  The call object must be further initialized to be usable.
     */
    static CallObject* create(JSContext* cx, HandleShape shape, HandleObjectGroup group);

    /*
     * Construct a bare-bones call object given a shape and make it into
     * a singleton.  The call object must be initialized to be usable.
     */
    static CallObject* createSingleton(JSContext* cx, HandleShape shape);

    static CallObject* createTemplateObject(JSContext* cx, HandleScript script,
                                            HandleObject enclosing, gc::InitialHeap heap);

    static CallObject* createForFunction(JSContext* cx, HandleObject enclosing,
                                         HandleFunction callee);

    static CallObject* createForFunction(JSContext* cx, AbstractFramePtr frame);
    static CallObject* createForStrictEval(JSContext* cx, AbstractFramePtr frame);
    static CallObject* createHollowForDebug(JSContext* cx, HandleFunction callee);

    /* True if this is for a strict mode eval frame. */
    bool isForEval() const {
        if (is<ModuleEnvironmentObject>())
            return false;
        MOZ_ASSERT(getFixedSlot(CALLEE_SLOT).isObjectOrNull());
        MOZ_ASSERT_IF(getFixedSlot(CALLEE_SLOT).isObject(),
                      getFixedSlot(CALLEE_SLOT).toObject().is<JSFunction>());
        return getFixedSlot(CALLEE_SLOT).isNull();
    }

    /*
     * When an aliased formal (var accessed by nested closures) is also
     * aliased by the arguments object, it must of course exist in one
     * canonical location and that location is always the CallObject. For this
     * to work, the ArgumentsObject stores special MagicValue in its array for
     * forwarded-to- CallObject variables. This MagicValue's payload is the
     * slot of the CallObject to access.
     */
    const Value& aliasedFormalFromArguments(const Value& argsValue) {
        return getSlot(ArgumentsObject::SlotFromMagicScopeSlotValue(argsValue));
    }
    inline void setAliasedFormalFromArguments(JSContext* cx, const Value& argsValue, jsid id,
                                              const Value& v);

    /*
     * Returns the function for which this CallObject was created. (This may
     * only be called if !isForEval.)
     */
    JSFunction& callee() const {
        return getReservedSlot(CALLEE_SLOT).toObject().as<JSFunction>();
    }

    /* For jit access. */
    static size_t offsetOfCallee() {
        return getFixedSlotOffset(CALLEE_SLOT);
    }

    static size_t calleeSlot() {
        return CALLEE_SLOT;
    }
};

class ModuleEnvironmentObject : public VarEnvironmentObject
{
    static const uint32_t MODULE_SLOT = 1;

    static const ObjectOps objectOps_;

  public:
    static const Class class_;

    static const uint32_t RESERVED_SLOTS = 2;

    static ModuleEnvironmentObject* create(ExclusiveContext* cx, HandleModuleObject module);
    ModuleObject& module();
    IndirectBindingMap& importBindings();

    bool createImportBinding(JSContext* cx, HandleAtom importName, HandleModuleObject module,
                             HandleAtom exportName);

    bool hasImportBinding(HandlePropertyName name);

    bool lookupImport(jsid name, ModuleEnvironmentObject** envOut, Shape** shapeOut);

  private:
    static bool lookupProperty(JSContext* cx, HandleObject obj, HandleId id,
                               MutableHandleObject objp, MutableHandleShape propp);
    static bool hasProperty(JSContext* cx, HandleObject obj, HandleId id, bool* foundp);
    static bool getProperty(JSContext* cx, HandleObject obj, HandleValue receiver, HandleId id,
                            MutableHandleValue vp);
    static bool setProperty(JSContext* cx, HandleObject obj, HandleId id, HandleValue v,
                            HandleValue receiver, JS::ObjectOpResult& result);
    static bool getOwnPropertyDescriptor(JSContext* cx, HandleObject obj, HandleId id,
                                         MutableHandle<PropertyDescriptor> desc);
    static bool deleteProperty(JSContext* cx, HandleObject obj, HandleId id,
                               ObjectOpResult& result);
    static bool enumerate(JSContext* cx, HandleObject obj, AutoIdVector& properties,
                          bool enumerableOnly);
};

typedef Rooted<ModuleEnvironmentObject*> RootedModuleEnvironmentObject;
typedef Handle<ModuleEnvironmentObject*> HandleModuleEnvironmentObject;
typedef MutableHandle<ModuleEnvironmentObject*> MutableHandleModuleEnvironmentObject;

class LexicalEnvironmentObject : public EnvironmentObject
{
    // Global and non-syntactic lexical environments need to store a 'this'
    // value and all other lexical environments have a fixed shape and store a
    // backpointer to the LexicalScope.
    //
    // Since the two sets are disjoint, we only use one slot to save space.
    static const unsigned THIS_VALUE_OR_SCOPE_SLOT = 1;

  public:
    static const unsigned RESERVED_SLOTS = 2;
    static const Class class_;

  private:
    static LexicalEnvironmentObject* create(JSContext* cx, HandleShape shape,
                                            HandleObject enclosing,
                                            gc::InitialHeap heap);

    void initThisValue(JSObject* obj) {
        MOZ_ASSERT(isGlobal() || !isSyntactic());
        initReservedSlot(THIS_VALUE_OR_SCOPE_SLOT, GetThisValue(obj));
    }

    void initScope(LexicalScope* scope) {
        MOZ_ASSERT(!isGlobal() && isSyntactic());
        initReservedSlot(THIS_VALUE_OR_SCOPE_SLOT, PrivateGCThingValue(scope));
    }

  public:
    static LexicalEnvironmentObject* create(JSContext* cx, Handle<LexicalScope*> scope,
                                            HandleObject enclosing, gc::InitialHeap heap);

    static LexicalEnvironmentObject* create(JSContext* cx, Handle<LexicalScope*> scope,
                                            AbstractFramePtr frame);
    static LexicalEnvironmentObject* createGlobal(JSContext* cx, Handle<GlobalObject*> global);
    static LexicalEnvironmentObject* createNonSyntactic(JSContext* cx, HandleObject enclosing);
    static LexicalEnvironmentObject* createHollowForDebug(JSContext* cx,
                                                          Handle<LexicalScope*> scope);

    // Create a new ClonedBlockObject with the same enclosing scope and
    // variable values as this.
    static LexicalEnvironmentObject* clone(JSContext* cx, Handle<LexicalEnvironmentObject*> env);

    // Copy in all the unaliased formals and locals.
    static bool copyUnaliasedValues(JSContext* cx, Handle<LexicalEnvironmentObject*> env,
                                    AbstractFramePtr frame);

    // For non-extensible lexical environments, the LexicalScope that created
    // this environment. Otherwise asserts.
    LexicalScope& scope() const {
        Value v = getReservedSlot(THIS_VALUE_OR_SCOPE_SLOT);
        MOZ_ASSERT(!isExtensible() && v.isPrivateGCThing());
        return *static_cast<LexicalScope*>(v.toGCThing());
    }

    // Is this the global lexical scope?
    bool isGlobal() const {
        return enclosingEnvironment().is<GlobalObject>();
    }

    GlobalObject& global() const {
        return enclosingEnvironment().as<GlobalObject>();
    }

    // Global and non-syntactic lexical scopes are extensible. All other
    // lexical scopes are not.
    bool isExtensible() const;

    // Is this a syntactic (i.e. corresponds to a source text) lexical
    // environment?
    bool isSyntactic() const {
        return !isExtensible() || isGlobal();
    }

    // For extensible lexical environments, the 'this' value for its
    // scope. Otherwise asserts.
    Value thisValue() const;
};

class DeclEnvObject : public LexicalEnvironmentObject
{
    static DeclEnvObject* create(JSContext* cx, HandleFunction canonicalFun,
                                 HandleObject enclosing, gc::InitialHeap heap);

  public:
    static DeclEnvObject* createTemplateObject(JSContext* cx, HandleFunction canonicalFun,
                                               gc::InitialHeap heap);

    static DeclEnvObject* create(JSContext* cx, HandleFunction fun);

    // For JITs.
    static size_t lambdaSlot();
};

// A non-syntactic dynamic scope object that captures non-lexical
// bindings. That is, a scope object that captures both qualified var
// assignments and unqualified bareword assignments. Its parent is always the
// global lexical environment.
//
// This is used in ExecuteInGlobalAndReturnScope and sits in front of the
// global scope to capture 'var' and bareword asignments.
class NonSyntacticVariablesObject : public EnvironmentObject
{
  public:
    static const unsigned RESERVED_SLOTS = 1;
    static const Class class_;

    static NonSyntacticVariablesObject* create(JSContext* cx);
};

// With environment objects on the run-time environment chain.
class WithEnvironmentObject : public EnvironmentObject
{
    static const unsigned OBJECT_SLOT = 1;
    static const unsigned THIS_SLOT = 2;
    static const unsigned SCOPE_SLOT = 3;

  public:
    static const unsigned RESERVED_SLOTS = 4;
    static const Class class_;

    static WithEnvironmentObject* create(JSContext* cx, HandleObject object, HandleObject enclosing,
                                         Handle<WithScope*> scope);
    static WithEnvironmentObject* createNonSyntactic(JSContext* cx, HandleObject object,
                                                     HandleObject enclosing);

    /* Return the 'o' in 'with (o)'. */
    JSObject& object() const;

    /* Return object for GetThisValue. */
    JSObject* withThis() const;

    /*
     * Return whether this object is a syntactic with object.  If not, this is
     * a With object we inserted between the outermost syntactic scope and the
     * global object to wrap the environment chain someone explicitly passed
     * via JSAPI to CompileFunction or script evaluation.
     */
    bool isSyntactic() const;

    // For syntactic with environment objects, the with scope.
    WithScope& scope() const;

    static inline size_t objectSlot() {
        return OBJECT_SLOT;
    }

    static inline size_t thisSlot() {
        return THIS_SLOT;
    }
};

// Internal scope object used by JSOP_BINDNAME upon encountering an
// uninitialized lexical slot or an assignment to a 'const' binding.
//
// ES6 lexical bindings cannot be accessed in any way (throwing
// ReferenceErrors) until initialized. Normally, NAME operations
// unconditionally check for uninitialized lexical slots. When getting or
// looking up names, this can be done without slowing down normal operations
// on the return value. When setting names, however, we do not want to pollute
// all set-property paths with uninitialized lexical checks. For setting names
// (i.e. JSOP_SETNAME), we emit an accompanying, preceding JSOP_BINDNAME which
// finds the right scope on which to set the name. Moreover, when the name on
// the scope is an uninitialized lexical, we cannot throw eagerly, as the spec
// demands that the error be thrown after evaluating the RHS of
// assignments. Instead, this sentinel scope object is pushed on the stack.
// Attempting to access anything on this scope throws the appropriate
// ReferenceError.
//
// ES6 'const' bindings induce a runtime error when assigned to outside
// of initialization, regardless of strictness.
class RuntimeLexicalErrorObject : public EnvironmentObject
{
    static const unsigned ERROR_SLOT = 1;

  public:
    static const unsigned RESERVED_SLOTS = 2;
    static const Class class_;

    static RuntimeLexicalErrorObject* create(JSContext* cx, HandleObject enclosing,
                                             unsigned errorNumber);

    unsigned errorNumber() {
        return getReservedSlot(ERROR_SLOT).toInt32();
    }
};


/*****************************************************************************/

// A environment iterator describes the active environments starting from an
// environment, scope pair. This pair may be derived from the current point of
// execution in a frame. If derived in such a fashion, the EnvironmentIter
// tracks whether the current scope is within the extent of this initial
// frame.  Here, "frame" means a single activation of: a function, eval, or
// global code.
class MOZ_RAII EnvironmentIter
{
    Rooted<ScopeIter> si_;
    RootedObject env_;
    AbstractFramePtr frame_;

    void incrementScopeIter();
    void settle();

    // No value semantics.
    EnvironmentIter(const EnvironmentIter& ei) = delete;

  public:
    // Constructing from a copy of an existing EnvironmentIter.
    EnvironmentIter(JSContext* cx, const EnvironmentIter& ei
                    MOZ_GUARD_OBJECT_NOTIFIER_PARAM);

    // Constructing from an environment, scope pair. All environments
    // considered not to be withinInitialFrame, since no frame is given.
    EnvironmentIter(JSContext* cx, JSObject* env, Scope* scope
                    MOZ_GUARD_OBJECT_NOTIFIER_PARAM);

    // Constructing from a frame. Places the EnvironmentIter on the innermost
    // environment at pc.
    EnvironmentIter(JSContext* cx, AbstractFramePtr frame, jsbytecode* pc
                    MOZ_GUARD_OBJECT_NOTIFIER_PARAM);

    bool done() const {
        return si_.done();
    }

    explicit operator bool() const {
        return !done();
    }

    void operator++(int) {
        incrementScopeIter();
        settle();
    }

    EnvironmentIter& operator++() {
        operator++(1);
        return *this;
    }

    // If done():
    JSObject& enclosingEnvironment() const;

    // If !done():
    bool hasNonSyntacticEnvironmentObject() const;

    bool hasSyntacticEnvironment() const {
        return si_.hasSyntacticEnvironment();
    }

    bool hasAnyEnvironmentObject() const {
        return hasNonSyntacticEnvironmentObject() || hasSyntacticEnvironment();
    }

    EnvironmentObject& environment() const {
        MOZ_ASSERT(hasAnyEnvironmentObject());
        return env_->as<EnvironmentObject>();
    }

    Scope& scope() const {
        return *si_.scope();
    }

    Scope* maybeScope() const {
        if (si_)
            return si_.scope();
        return nullptr;
    }

    JSFunction& callee() const {
        return env_->as<CallObject>().callee();
    }

    bool withinInitialFrame() const {
        return !!frame_;
    }

    AbstractFramePtr initialFrame() const {
        MOZ_ASSERT(withinInitialFrame());
        return frame_;
    }

    AbstractFramePtr maybeInitialFrame() const {
        return frame_;
    }

    MOZ_DECL_USE_GUARD_OBJECT_NOTIFIER
};

// The key in MissingEnvironmentMap. For live frames, maps live frames to
// their synthesized environments. For completely optimized-out environments,
// maps the Scope to their synthesized environments. The env we synthesize for
// Scopes are read-only, and we never use their parent links, so they don't
// need to be distinct.
//
// That is, completely optimized out environments can't be distinguished by
// frame. Note that even if the frame corresponding to the Scope is live on
// the stack, it is unsound to synthesize an environment from that live
// frame. In other words, the provenance of the environment chain is from
// allocated closures (i.e., allocation sites) and is irrecoverable from
// simple stack inspection (i.e., call sites).
class MissingEnvironmentKey
{
    friend class LiveEnvironmentVal;

    AbstractFramePtr frame_;
    Scope* scope_;

  public:
    explicit MissingEnvironmentKey(const EnvironmentIter& ei)
      : frame_(ei.maybeInitialFrame()),
        scope_(ei.maybeScope())
    { }

    AbstractFramePtr frame() const { return frame_; }
    Scope* scope() const { return scope_; }

    void updateScope(Scope* scope) { scope_ = scope; }
    void updateFrame(AbstractFramePtr frame) { frame_ = frame; }

    // For use as hash policy.
    typedef MissingEnvironmentKey Lookup;
    static HashNumber hash(MissingEnvironmentKey sk);
    static bool match(MissingEnvironmentKey sk1, MissingEnvironmentKey sk2);
    bool operator!=(const MissingEnvironmentKey& other) const {
        return frame_ != other.frame_ || scope_ != other.scope_;
    }
    static void rekey(MissingEnvironmentKey& k, const MissingEnvironmentKey& newKey) {
        k = newKey;
    }
};

// The value in LiveEnvironmentMap, mapped from by live environment objects.
class LiveEnvironmentVal
{
    friend class DebugEnvironments;
    friend class MissingEnvironmentKey;

    AbstractFramePtr frame_;
    HeapPtr<Scope*> scope_;

    static void staticAsserts();

  public:
    explicit LiveEnvironmentVal(const EnvironmentIter& ei)
      : frame_(ei.initialFrame()),
        scope_(ei.maybeScope())
    { }

    AbstractFramePtr frame() const { return frame_; }
    Scope* scope() const { return scope_; }

    void updateFrame(AbstractFramePtr frame) { frame_ = frame; }

    bool needsSweep();
};


/*****************************************************************************/

/*
 * Debug scope objects
 *
 * The debugger effectively turns every opcode into a potential direct eval.
 * Naively, this would require creating a EnvironmentObject for every
 * call/block scope and using JSOP_GETALIASEDVAR for every access. To optimize
 * this, the engine assumes there is no debugger and optimizes scope access
 * and creation accordingly. When the debugger wants to perform an unexpected
 * eval-in-frame (or other, similar dynamic-scope-requiring operations),
 * fp->scopeChain is now incomplete: it may not contain all, or any, of the
 * EnvironmentObjects to represent the current scope.
 *
 * To resolve this, the debugger first calls GetDebugEnvironmentFor* to
 * synthesize a "debug scope chain". A debug scope chain is just a chain of
 * objects that fill in missing scopes and protect the engine from unexpected
 * access. (The latter means that some debugger operations, like redefining a
 * lexical binding, can fail when a true eval would succeed.) To do both of
 * these things, GetDebugEnvironmentFor* creates a new proxy
 * DebugEnvironmentProxy to sit in front of every existing EnvironmentObject.
 *
 * GetDebugEnvironmentFor* ensures the invariant that the same
 * DebugEnvironmentProxy is always produced for the same underlying scope
 * (optimized or not!). This is maintained by some bookkeeping information
 * stored in DebugEnvironments.
 */

extern JSObject*
GetDebugEnvironmentForFunction(JSContext* cx, HandleFunction fun);

extern JSObject*
GetDebugEnvironmentForFrame(JSContext* cx, AbstractFramePtr frame, jsbytecode* pc);

extern JSObject*
GetDebugEnvironmentForGlobalLexicalScope(JSContext* cx);

/* Provides debugger access to a scope. */
class DebugEnvironmentProxy : public ProxyObject
{
    /*
     * The enclosing scope on the dynamic scope chain. This slot is analogous
     * to the SCOPE_CHAIN_SLOT of a EnvironmentObject.
     */
    static const unsigned ENCLOSING_EXTRA = 0;

    /*
     * NullValue or a dense array holding the unaliased variables of a function
     * frame that has been popped.
     */
    static const unsigned SNAPSHOT_EXTRA = 1;

  public:
    static DebugEnvironmentProxy* create(JSContext* cx, EnvironmentObject& env,
                                         HandleObject enclosing);

    EnvironmentObject& environment() const;
    JSObject& enclosingEnvironment() const;

    /* May only be called for proxies to function call objects. */
    ArrayObject* maybeSnapshot() const;
    void initSnapshot(ArrayObject& snapshot);

    /* Currently, the 'declarative' scopes are Call and Block. */
    bool isForDeclarative() const;

    // Get a property by 'id', but returns sentinel values instead of throwing
    // on exceptional cases.
    bool getMaybeSentinelValue(JSContext* cx, HandleId id, MutableHandleValue vp);

    // Returns true iff this is a function scope with its own this-binding
    // (all functions except arrow functions and generator expression lambdas).
    bool isFunctionEnvironmentWithThis();

    // Does this debug scope not have a dynamic counterpart or was never live
    // (and thus does not have a synthesized EnvironmentObject or a snapshot)?
    bool isOptimizedOut() const;
};

/* Maintains per-compartment debug scope bookkeeping information. */
class DebugEnvironments
{
    /* The map from (non-debug) scopes to debug scopes. */
    ObjectWeakMap proxiedEnvs;

    /*
     * The map from live frames which have optimized-away scopes to the
     * corresponding debug scopes.
     */
    typedef HashMap<MissingEnvironmentKey,
                    ReadBarrieredDebugEnvironmentProxy,
                    MissingEnvironmentKey,
                    RuntimeAllocPolicy> MissingEnvironmentMap;
    MissingEnvironmentMap missingEnvs;

    /*
     * The map from scope objects of live frames to the live frame. This map
     * updated lazily whenever the debugger needs the information. In between
     * two lazy updates, liveScopes becomes incomplete (but not invalid, onPop*
     * removes scopes as they are popped). Thus, two consecutive debugger lazy
     * updates of liveScopes need only fill in the new scopes.
     */
    typedef GCHashMap<ReadBarriered<JSObject*>,
                      LiveEnvironmentVal,
                      MovableCellHasher<ReadBarriered<JSObject*>>,
                      RuntimeAllocPolicy> LiveEnvironmentMap;
    LiveEnvironmentMap liveEnvs;
    static MOZ_ALWAYS_INLINE void liveScopesPostWriteBarrier(JSRuntime* rt,
                                                             LiveEnvironmentMap* map,
                                                             EnvironmentObject* key);

  public:
    explicit DebugEnvironments(JSContext* c);
    ~DebugEnvironments();

  private:
    bool init();

    static DebugEnvironments* ensureCompartmentData(JSContext* cx);

  public:
    void mark(JSTracer* trc);
    void sweep(JSRuntime* rt);
#ifdef JS_GC_ZEAL
    void checkHashTablesAfterMovingGC(JSRuntime* rt);
#endif

    static DebugEnvironmentProxy* hasDebugEnvironment(JSContext* cx, EnvironmentObject& env);
    static bool addDebugEnvironment(JSContext* cx, EnvironmentObject& env,
                                    DebugEnvironmentProxy& debugEnv);

    static DebugEnvironmentProxy* hasDebugEnvironment(JSContext* cx, const EnvironmentIter& ei);
    static bool addDebugEnvironment(JSContext* cx, const EnvironmentIter& ei,
                              DebugEnvironmentProxy& debugEnv);

    static bool updateLiveEnvironments(JSContext* cx);
    static LiveEnvironmentVal* hasLiveEnvironment(EnvironmentObject& env);
    static void unsetPrevUpToDateUntil(JSContext* cx, AbstractFramePtr frame);

    // When a frame bails out from Ion to Baseline, there might be missing
    // envs keyed on, and live envs containing, the old
    // RematerializedFrame. Forward those values to the new BaselineFrame.
    static void forwardLiveFrame(JSContext* cx, AbstractFramePtr from, AbstractFramePtr to);

    // In debug-mode, these must be called whenever exiting a scope that might
    // have stack-allocated locals.
    static void onPopCall(AbstractFramePtr frame, JSContext* cx);
    static void onPopBlock(JSContext* cx, const EnvironmentIter& ei);
    static void onPopBlock(JSContext* cx, AbstractFramePtr frame, jsbytecode* pc);
    static void onPopWith(AbstractFramePtr frame);
    static void onPopStrictEval(AbstractFramePtr frame);
    static void onCompartmentUnsetIsDebuggee(JSCompartment* c);
};

}  /* namespace js */

template <>
inline bool
JSObject::is<js::VarEnvironmentObject>() const
{
    return is<js::CallObject>();
}

template <>
inline bool
JSObject::is<js::EnvironmentObject>() const
{
    return is<js::VarEnvironmentObject>() ||
           is<js::LexicalEnvironmentObject>();
}

template<>
bool
JSObject::is<js::DebugEnvironmentProxy>() const;

namespace js {

inline bool
IsSyntacticEnvironment(JSObject* scope)
{
    if (!scope->is<EnvironmentObject>())
        return false;

    if (scope->is<WithEnvironmentObject>())
        return scope->as<WithEnvironmentObject>().isSyntactic();

    if (scope->is<LexicalEnvironmentObject>())
        return scope->as<LexicalEnvironmentObject>().isSyntactic();

    if (scope->is<NonSyntacticVariablesObject>())
        return false;

    return true;
}

inline bool
IsExtensibleLexicalEnvironment(JSObject* env)
{
    return env->is<LexicalEnvironmentObject>() &&
           env->as<LexicalEnvironmentObject>().isExtensible();
}

inline bool
IsGlobalLexicalEnvironment(JSObject* env)
{
    return env->is<LexicalEnvironmentObject>() &&
           env->as<LexicalEnvironmentObject>().isGlobal();
}

extern bool
CreateObjectsForEnvironmentChain(JSContext* cx, AutoObjectVector& chain,
                                 HandleObject terminatingEnv,
                                 MutableHandleObject envObj);

ModuleEnvironmentObject* GetModuleEnvironmentForScript(JSScript* script);

bool GetThisValueForDebuggerMaybeOptimizedOut(JSContext* cx, AbstractFramePtr frame, jsbytecode* pc,
                                              MutableHandleValue res);

bool CheckVarNameConflict(JSContext* cx, Handle<LexicalEnvironmentObject*> lexicalEnv,
                          HandlePropertyName name);

bool CheckLexicalNameConflict(JSContext* cx, Handle<LexicalEnvironmentObject*> lexicalScope,
                              HandleObject varObj, HandlePropertyName name);

bool CheckGlobalDeclarationConflicts(JSContext* cx, HandleScript script,
                                     Handle<LexicalEnvironmentObject*> lexicalScope,
                                     HandleObject varObj);

bool CheckEvalDeclarationConflicts(JSContext* cx, HandleScript script,
                                   HandleObject scopeChain, HandleObject varObj);

#ifdef DEBUG
bool
AnalyzeEntrainedVariables(JSContext* cx, HandleScript script);
#endif

} // namespace js

#endif /* vm_ScopeObject_h */

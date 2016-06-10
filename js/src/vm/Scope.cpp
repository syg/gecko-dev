/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "vm/Scope.h"

#include "jsscript.h"
#include "gc/Allocator.h"
#include "vm/Runtime.h"
#include "vm/ScopeObject.h"

using namespace js;

using mozilla::Maybe;
using mozilla::Some;
using mozilla::Nothing;

const char*
js::BindingKindString(BindingKind kind)
{
    switch (kind) {
      case BindingKind::FormalParameter:
        return "formal parameter";
      case BindingKind::Var:
        return "var";
      case BindingKind::Let:
        return "let";
      case BindingKind::Const:
        return "const";
    }
    MOZ_CRASH("Bad BindingKind");
}

const char*
js::ScopeKindString(ScopeKind kind)
{
    switch (kind) {
      case ScopeKind::Function:
        return "function";
      case ScopeKind::ParameterDefaults:
        return "parameter defaults";
      case ScopeKind::Lexical:
        return "lexical";
      case ScopeKind::Catch:
        return "catch";
      case ScopeKind::With:
        return "with";
      case ScopeKind::Eval:
        return "eval";
      case ScopeKind::StrictEval:
        return "strict eval";
      case ScopeKind::Global:
        return "global";
      case ScopeKind::NonSyntactic:
        return "non-syntactic";
      case ScopeKind::Module:
        return "module";
    }
    MOZ_CRASH("Bad ScopeKind");
}

static Shape*
EmptyEnvironmentShape(ExclusiveContext* cx, const Class* cls, uint32_t numSlots,
                      uint32_t baseShapeFlags)
{
    // Put as many slots into the object header as possible.
    uint32_t numFixed = gc::GetGCKindSlots(gc::GetGCObjectKind(numSlots));
    return EmptyShape::getInitialShape(cx, cls, TaggedProto(nullptr), numFixed,
                                       baseShapeFlags);
}

static Shape*
NextEnvironmentShape(ExclusiveContext* cx, JSAtom* name, BindingKind bindKind, uint32_t slot,
                     StackBaseShape& stackBase, HandleShape shape)
{
    UnownedBaseShape* base = BaseShape::getUnowned(cx, stackBase);
    if (!base)
        return nullptr;

    unsigned attrs = JSPROP_PERMANENT | JSPROP_ENUMERATE |
                     (bindKind == BindingKind::Const ? JSPROP_READONLY : 0);
    jsid id = NameToId(name->asPropertyName());
    Rooted<StackShape> child(cx, StackShape(base, id, slot, attrs, 0));
    return cx->compartment()->propertyTree.getChild(cx, shape, child);
}

static Shape*
CreateEnvironmentShape(ExclusiveContext* cx, BindingIter& bi, const Class* cls,
                       uint32_t numSlots, uint32_t baseShapeFlags)
{
    RootedShape shape(cx, EmptyEnvironmentShape(cx, cls, numSlots, baseShapeFlags));
    if (!shape)
        return nullptr;

    StackBaseShape stackBase(cx, cls, baseShapeFlags);
    for (; bi; bi++) {
        BindingLocation loc = bi.location();
        if (loc.kind() == BindingLocation::Kind::Environment) {
            shape = NextEnvironmentShape(cx, bi.name(), bi.kind(), loc.slot(), stackBase, shape);
            if (!shape)
                return nullptr;
        }
    }

    return shape;
}

template <typename ScopeData>
static ScopeData*
CopyBindingData(ExclusiveContext* cx, ScopeData* data, size_t dataSize)
{
    // The copy itself copies JSAtom* bytes and is not GC safe unless in the
    // presence of an AutoKeepAtoms.
    MOZ_ASSERT(cx->compartment()->runtimeFromAnyThread()->keepAtoms());

    uint8_t* copyBytes = cx->zone()->pod_malloc<uint8_t>(dataSize);
    if (copyBytes)
        mozilla::PodCopy<uint8_t>(copyBytes, reinterpret_cast<uint8_t*>(data), dataSize);
    return reinterpret_cast<ScopeData*>(copyBytes);
}

template <typename ScopeData>
static ScopeData*
CopyBindingData(ExclusiveContext* cx, BindingIter& bi, ScopeData* data, size_t dataSize,
              const Class* cls, uint32_t baseShapeFlags, Shape** envShape)
{
    // Copy a fresh BindingIter for use below.
    BindingIter freshBi(bi);

    // Iterate through all bindings. This counts the number of environment
    // slots needed and computes the maximum frame slot.
    while (bi)
        bi++;
    data->nextFrameSlot = bi.nextFrameSlot();

    // Make a new environment shape if any environment slots were used.
    if (bi.nextEnvironmentSlot() == JSSLOT_FREE(cls)) {
        *envShape = nullptr;
    } else {
        *envShape = CreateEnvironmentShape(cx, freshBi, cls, bi.nextEnvironmentSlot(),
                                           baseShapeFlags);
        if (!*envShape)
            return nullptr;
    }

    return CopyBindingData(cx, data, dataSize);
}

template <typename ScopeData>
static ScopeData*
NewEmptyScopeData(ExclusiveContext* cx, size_t dataSize)
{
    uint8_t* bytes = cx->zone()->pod_calloc<uint8_t>(dataSize);
    return reinterpret_cast<ScopeData*>(bytes);
}

/* static */ Scope*
Scope::create(ExclusiveContext* cx, ScopeKind kind, Scope* enclosing, Shape* environmentShape,
              uintptr_t data)
{
    Scope* scope = Allocate<Scope>(cx);
    if (scope)
        new (scope) Scope(kind, enclosing, environmentShape, data);
    return scope;
}

uint32_t
Scope::chainLength() const
{
    uint32_t length = 0;
    for (ScopeIter si(const_cast<Scope*>(this)); si; si++)
        length++;
    return length;
}

uint32_t
Scope::environmentChainLength() const
{
    uint32_t length = 0;
    for (ScopeIter si(const_cast<Scope*>(this)); si; si++) {
        if (si.hasSyntacticEnvironment())
            length++;
    }
    return length;
}

Shape*
Scope::maybeCloneEnvironmentShape(JSContext* cx)
{
    // Clone the environment shape if cloning into a different compartment.
    if (environmentShape_ && environmentShape_->compartment() != cx->compartment()) {
        BindingIter bi(this);
        return CreateEnvironmentShape(cx, bi,
                                      environmentShape_->getObjectClass(),
                                      environmentShape_->slot() + 1,
                                      environmentShape_->getObjectFlags());
    }
    return environmentShape_;
}

Scope*
Scope::clone(JSContext* cx, Scope* enclosing)
{
    MOZ_ASSERT(!is<FunctionScope>() && !is<GlobalScope>(),
               "FunctionScopes and GlobalScopes should use the class-specific clone.");
    MOZ_ASSERT_IF(is<LexicalScope>(), as<LexicalScope>().computeFirstFrameSlot() ==
                                      LexicalScope::computeNextFrameSlot(enclosing));

    Shape* envShape = maybeCloneEnvironmentShape(cx);
    if (!envShape)
        return nullptr;

    Scope* clone = create(cx, kind_, enclosing, envShape, data_);
    if (!clone)
        return nullptr;

    reinterpret_cast<RefCountedData*>(data_)->addRef();
    return clone;
}

void
Scope::RefCountedData::release(FreeOp* fop)
{
    MOZ_ASSERT(refCount > 0);
    if (--refCount == 0)
        fop->free_(this);
}

void
Scope::finalize(FreeOp* fop)
{
    if (data_) {
        if (is<FunctionScope>()) {
            as<FunctionScope>().data().bindings->release(fop);
            fop->free_(reinterpret_cast<void*>(data_));
        } else {
            reinterpret_cast<RefCountedData*>(data_)->release(fop);
        }
        data_ = 0;
    }
}

void
Scope::dump() const
{
    for (ScopeIter si(const_cast<Scope*>(this)); si; si++) {
        fprintf(stdout, "%s [%p]", ScopeKindString(si.kind()), si.scope());
        if (si.scope()->enclosing())
            fprintf(stdout, " -> ");
    }
}

/* static */ uint32_t
LexicalScope::computeNextFrameSlot(Scope* start)
{
    for (Scope* it = start; it; it = it->enclosing()) {
        if (it->is<LexicalScope>())
            return it->as<LexicalScope>().nextFrameSlot();
        if (it->is<FunctionScope>())
            return it->as<FunctionScope>().nextFrameSlot();
        if (it->is<EvalScope>())
            return it->as<EvalScope>().nextFrameSlot();
    }
    return 0;
}

/* static */ LexicalScope*
LexicalScope::create(ExclusiveContext* cx, ScopeKind kind, BindingData* data,
                     uint32_t firstFrameSlot, Scope* enclosing)
{
    MOZ_ASSERT(data, "LexicalScopes should not be created if there are no bindings.");
    MOZ_ASSERT(firstFrameSlot == computeNextFrameSlot(enclosing));

    // The data that's passed in is from the frontend and is LifoAlloc'd or is
    // from Scope::copy. Copy it now that we're creating a permanent VM scope.
    Shape* envShape = nullptr;
    BindingData* copy;
    BindingIter bi(*data, firstFrameSlot);
    copy = CopyBindingData(cx, bi, data, sizeOfBindingData(data->length),
                           &ClonedBlockObject::class_,
                           BaseShape::NOT_EXTENSIBLE | BaseShape::DELEGATE,
                           &envShape);
    if (!copy)
        return nullptr;

    Scope* scope = Scope::create(cx, kind, enclosing, envShape, reinterpret_cast<uintptr_t>(copy));
    if (!scope)
        js_free(copy);
    copy->addRef();
    return &scope->as<LexicalScope>();
}

/* static */ FunctionScope*
FunctionScope::create(ExclusiveContext* cx, BindingData* bindings, uint32_t firstFrameSlot,
                      JSFunction* fun, Scope* enclosing)
{
    MOZ_ASSERT_IF(enclosing, firstFrameSlot == computeNextFrameSlot(enclosing));
    MOZ_ASSERT(fun->isTenured());

    Data* data = NewEmptyScopeData<Data>(cx, sizeof(Data));
    if (!data)
        return nullptr;

    data->canonicalFunction.init(fun);

    // The data that's passed in is from the frontend and is LifoAlloc'd or is
    // from Scope::copy. Copy it now that we're creating a permanent VM scope.
    Shape* envShape = nullptr;
    if (bindings) {
        BindingIter bi(*bindings, firstFrameSlot);
        data->bindings = CopyBindingData(cx, bi, bindings, sizeOfBindingData(bindings->length),
                                         &CallObject::class_,
                                         BaseShape::QUALIFIED_VAROBJ | BaseShape::DELEGATE,
                                         &envShape);
    } else {
        data->bindings = NewEmptyScopeData<BindingData>(cx, sizeOfBindingData(1));
    }

    if (!data->bindings)
        return nullptr;

    Scope* scope = Scope::create(cx, ScopeKind::Function, enclosing, envShape,
                                 reinterpret_cast<uintptr_t>(data));
    if (!scope)
        js_free(data);
    data->bindings->addRef();
    return &scope->as<FunctionScope>();
}

FunctionScope*
FunctionScope::clone(JSContext* cx, JSFunction* fun, Scope* enclosing)
{
    MOZ_ASSERT_IF(enclosing, computeFirstFrameSlot() == computeNextFrameSlot(enclosing));
    MOZ_ASSERT(fun != canonicalFunction());

    Data* dataClone = NewEmptyScopeData<Data>(cx, sizeof(Data));
    if (!dataClone)
        return nullptr;

    dataClone->canonicalFunction.init(fun);
    dataClone->bindings = data().bindings;

    Shape* envShape = maybeCloneEnvironmentShape(cx);
    if (!envShape)
        return nullptr;

    Scope* clone = Scope::create(cx, kind(), enclosing, envShape,
                                 reinterpret_cast<uintptr_t>(dataClone));
    if (!clone)
        return nullptr;

    dataClone->bindings->addRef();
    return &clone->as<FunctionScope>();
}

JSScript*
FunctionScope::script() const
{
    return canonicalFunction()->nonLazyScript();
}

/* static */ GlobalScope*
GlobalScope::create(ExclusiveContext* cx, ScopeKind kind, BindingData* data)
{
    // The data that's passed in is from the frontend and is LifoAlloc'd or is
    // from Scope::copy. Copy it now that we're creating a permanent VM scope.
    BindingData* copy;
    if (data) {
        // The global scope has no environment shape. Its environment is the
        // global lexical scope and the global object or non-syntactic objects
        // created by embedding, all of which are not only extensible but may
        // have names on them deleted.
        copy = CopyBindingData(cx, data, sizeOfBindingData(data->length));
    } else {
        copy = NewEmptyScopeData<BindingData>(cx, sizeOfBindingData(1));
    }

    if (!copy)
        return nullptr;

    Scope* scope = Scope::create(cx, kind, nullptr, nullptr, reinterpret_cast<uintptr_t>(copy));
    if (!scope)
        js_free(copy);
    copy->addRef();
    return &scope->as<GlobalScope>();
}

GlobalScope*
GlobalScope::clone(JSContext* cx, ScopeKind kind)
{
    Scope* clone = Scope::create(cx, kind, nullptr, nullptr, data_);
    if (!clone)
        return nullptr;

    data().addRef();
    return &clone->as<GlobalScope>();
}

/* static */ WithScope*
WithScope::create(ExclusiveContext* cx, Scope* enclosing)
{
    Scope* scope = Scope::create(cx, ScopeKind::With, enclosing, nullptr, 0);
    return static_cast<WithScope*>(scope);
}

/* static */ EvalScope*
EvalScope::create(ExclusiveContext* cx, ScopeKind scopeKind, BindingData* data, Scope* enclosing)
{
    // The data that's passed in is from the frontend and is LifoAlloc'd or is
    // from Scope::copy. Copy it now that we're creating a permanent VM scope.
    Shape* envShape = nullptr;
    BindingData* copy;
    if (data) {
        if (scopeKind == ScopeKind::StrictEval) {
            BindingIter bi(*data, true);
            copy = CopyBindingData(cx, bi, data, sizeOfBindingData(data->length),
                                   &CallObject::class_,
                                   BaseShape::QUALIFIED_VAROBJ | BaseShape::DELEGATE,
                                   &envShape);
        } else {
            copy = CopyBindingData(cx, data, sizeOfBindingData(data->length));
        }
    } else {
        copy = NewEmptyScopeData<BindingData>(cx, sizeOfBindingData(1));
    }

    if (!copy)
        return nullptr;

    Scope* scope = Scope::create(cx, scopeKind, enclosing, envShape,
                                 reinterpret_cast<uintptr_t>(copy));
    if (!scope)
        js_free(copy);
    copy->addRef();
    return &scope->as<EvalScope>();
}

/* static */ Scope*
EvalScope::nearestVarScopeForDirectEval(Scope* scope)
{
    for (ScopeIter si(scope); si; si++) {
        switch (si.kind()) {
          case ScopeKind::Function:
          case ScopeKind::ParameterDefaults:
            // Direct evals in parameter default expressions always get
            // their own var scopes. Note that the parameter defaults
            // isn't itself the var scope (conceptually a fresh one is
            // created for each default expression).
          case ScopeKind::Global:
          case ScopeKind::NonSyntactic:
            return scope;
          default:
            break;
        }
    }
    return nullptr;
}

bool
ScopeIter::hasSyntacticEnvironment() const
{
    switch (scope()->kind()) {
      case ScopeKind::Function:
      case ScopeKind::ParameterDefaults:
      case ScopeKind::Lexical:
      case ScopeKind::Catch:
      case ScopeKind::StrictEval:
        return !!environmentShape();

      case ScopeKind::With:
      case ScopeKind::Global:
      case ScopeKind::Module:
        return true;

      case ScopeKind::Eval:
      case ScopeKind::NonSyntactic:
        return false;
    }

    MOZ_CRASH("Bad ScopeKind");
}

BindingIter::BindingIter(JSScript* script)
  : BindingIter(script->bodyScope())
{ }

void
BindingIter::init(LexicalScope::BindingData& data, uint32_t firstFrameSlot)
{
    // Named lambdas have a separate environment for their name that. This
    // scope cannot have frame slots and sets its firstFrameSlot to
    // LOCALNO_LIMIT. Asking for the location of a non-closed-over callee will
    // assert.
    init(0, 0, 0, data.constStart,
         (firstFrameSlot == LOCALNO_LIMIT ? 0 : CanHaveFrameSlots) | CanHaveEnvironmentSlots,
         firstFrameSlot, JSSLOT_FREE(&ClonedBlockObject::class_),
         data.names, data.length);
}

void
BindingIter::init(FunctionScope::BindingData& data, uint32_t firstFrameSlot)
{
    init(data.nonPositionalFormalStart, data.varStart, data.length, 0,
         CanHaveArgumentSlots | CanHaveFrameSlots | CanHaveEnvironmentSlots,
         firstFrameSlot, JSSLOT_FREE(&CallObject::class_),
         data.names, data.length);
}

void
BindingIter::init(GlobalScope::BindingData& data)
{
    init(0, 0, data.letStart, data.constStart,
         CannotHaveSlots,
         UINT32_MAX, UINT32_MAX,
         data.names, data.length);
}

void
BindingIter::init(EvalScope::BindingData& data, bool strict)
{
    if (strict) {
        init(0, 0, data.length, data.length,
             CanHaveFrameSlots | CanHaveEnvironmentSlots,
             0, JSSLOT_FREE(&CallObject::class_),
             data.names, data.length);
    } else {
        init(0, 0, data.length, data.length,
             CannotHaveSlots, UINT32_MAX, UINT32_MAX,
             data.names, data.length);
    }
}

JS::ubi::Node::Size
JS::ubi::Concrete<Scope>::size(mozilla::MallocSizeOf mallocSizeOf) const
{
    Size size = js::gc::Arena::thingSize(get().asTenured().getAllocKind());
    // TODOshu sizes
    return size;
}

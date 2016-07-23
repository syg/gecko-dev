/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "vm/Scope.h"

#include "mozilla/ScopeExit.h"

#include "jsscript.h"
#include "builtin/ModuleObject.h"
#include "gc/Allocator.h"
#include "vm/EnvironmentObject.h"
#include "vm/Runtime.h"

using namespace js;

using mozilla::Maybe;
using mozilla::Some;
using mozilla::Nothing;
using mozilla::MakeScopeExit;

const char*
js::BindingKindString(BindingKind kind)
{
    switch (kind) {
      case BindingKind::Import:
        return "import";
      case BindingKind::FormalParameter:
        return "formal parameter";
      case BindingKind::Var:
        return "var";
      case BindingKind::Let:
        return "let";
      case BindingKind::Const:
        return "const";
      case BindingKind::NamedLambdaCallee:
        return "named lambda callee";
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
      case ScopeKind::NamedLambda:
        return "named lambda";
      case ScopeKind::StrictNamedLambda:
        return "strict named lambda";
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

    unsigned attrs = JSPROP_PERMANENT | JSPROP_ENUMERATE;
    switch (bindKind) {
      case BindingKind::Const:
      case BindingKind::NamedLambdaCallee:
        attrs |= JSPROP_READONLY;
        break;
      default:
        break;
    }

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

template <typename BindingData>
static BindingData*
CopyBindingData(ExclusiveContext* cx, Handle<BindingData*> data, size_t dataSize)
{
    MOZ_ASSERT(!data->notLifoAllocated);
    uint8_t* copyBytes = cx->zone()->pod_malloc<uint8_t>(dataSize);
    if (!copyBytes) {
        ReportOutOfMemory(cx);
        return nullptr;
    }

    mozilla::PodCopy<uint8_t>(copyBytes, reinterpret_cast<uint8_t*>(data.get()), dataSize);
    BindingData* copy = reinterpret_cast<BindingData*>(copyBytes);
    copy->notLifoAllocated = true;
    copy->addRef();

    return copy;
}

template <typename BindingData>
static BindingData*
CopyBindingData(ExclusiveContext* cx, BindingIter& bi, Handle<BindingData*> data, size_t dataSize,
                const Class* cls, uint32_t baseShapeFlags, MutableHandleShape envShape)
{
    // Copy a fresh BindingIter for use below.
    BindingIter freshBi(bi);

    // Iterate through all bindings. This counts the number of environment
    // slots needed and computes the maximum frame slot.
    while (bi)
        bi++;
    data->nextFrameSlot = bi.canHaveFrameSlots() ? bi.nextFrameSlot() : LOCALNO_LIMIT;

    // Make a new environment shape if any environment slots were used.
    if (bi.nextEnvironmentSlot() == JSSLOT_FREE(cls)) {
        envShape.set(nullptr);
    } else {
        envShape.set(CreateEnvironmentShape(cx, freshBi, cls, bi.nextEnvironmentSlot(),
                                            baseShapeFlags));
        if (!envShape)
            return nullptr;
    }

    return CopyBindingData(cx, data, dataSize);
}

template <typename ScopeData>
static ScopeData*
NewEmptyScopeData(ExclusiveContext* cx, size_t dataSize)
{
    uint8_t* bytes = cx->zone()->pod_calloc<uint8_t>(dataSize);
    if (!bytes)
        ReportOutOfMemory(cx);
    return reinterpret_cast<ScopeData*>(bytes);
}

template <typename BindingData>
static BindingData*
NewEmptyScopeBindingData(ExclusiveContext* cx, size_t dataSize)
{
    BindingData* bindings = NewEmptyScopeData<BindingData>(cx, dataSize);
    if (bindings) {
        bindings->notLifoAllocated = true;
        bindings->addRef();
    }
    return bindings;
}

static bool
XDRBindingName(XDRState<XDR_ENCODE>* xdr, BindingName* bindingName)
{
    JSContext* cx = xdr->cx();

    RootedAtom atom(cx, bindingName->name());
    if (!XDRAtom(xdr, &atom))
        return false;

    uint8_t closedOver = bindingName->closedOver();
    if (!xdr->codeUint8(&closedOver))
        return false;

    return true;
}

static bool
XDRBindingName(XDRState<XDR_DECODE>* xdr, BindingName* bindingName)
{
    JSContext* cx = xdr->cx();

    RootedAtom atom(cx);
    if (!XDRAtom(xdr, &atom))
        return false;

    uint8_t closedOver;
    if (!xdr->codeUint8(&closedOver))
        return false;

    *bindingName = BindingName(atom, closedOver);

    return true;
}

template <typename ConcreteScope, XDRMode mode>
/* static */ bool
Scope::XDRSizedBindingData(XDRState<mode>* xdr, Handle<ConcreteScope*> scope,
                           MutableHandle<typename ConcreteScope::BindingData*> data)
{
    MOZ_ASSERT(!data);

    JSContext* cx = xdr->cx();
    auto freeOnFailure = MakeScopeExit([&data, cx]() {
        if (mode == XDR_DECODE && data)
            data->release(cx->defaultFreeOp());
        data.set(nullptr);
    });

    uint32_t length;
    if (mode == XDR_ENCODE)
        length = scope->bindingData().length;
    if (!xdr->codeUint32(&length))
        return false;

    if (mode == XDR_ENCODE) {
        data.set(&scope->bindingData());
    } else {
        size_t lengthForSizeOf = length ? length : 1;
        size_t size = ConcreteScope::sizeOfBindingData(lengthForSizeOf);
        data.set(NewEmptyScopeBindingData<typename ConcreteScope::BindingData>(cx, size));
        if (!data)
            return false;
        data->length = length;
    }

    for (uint32_t i = 0; i < length; i++) {
        if (!XDRBindingName(xdr, &data->names[i]))
            return false;
    }

    freeOnFailure.release();
    return true;
}

/* static */ Scope*
Scope::create(ExclusiveContext* cx, ScopeKind kind, HandleScope enclosing, HandleShape envShape,
              uintptr_t data)
{
    Scope* scope = Allocate<Scope>(cx);
    if (scope)
        new (scope) Scope(kind, enclosing, envShape, data);
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
                                      environmentShape_->slotSpan(),
                                      environmentShape_->getObjectFlags());
    }
    return environmentShape_;
}

/* static */ Scope*
Scope::clone(JSContext* cx, HandleScope scope, HandleScope enclosing)
{
    MOZ_ASSERT(!scope->is<FunctionScope>() && !scope->is<GlobalScope>(),
               "FunctionScopes and GlobalScopes should use the class-specific clone.");

    RootedShape envShape(cx);
    if (scope->environmentShape()) {
        envShape = scope->maybeCloneEnvironmentShape(cx);
        if (!envShape)
            return nullptr;
    }

    Scope* clone = create(cx, scope->kind_, enclosing, envShape, scope->data_);
    if (!clone)
        return nullptr;

    reinterpret_cast<RefCountedData*>(scope->data_)->addRef();
    return clone;
}

void
Scope::RefCountedData::release(FreeOp* fop)
{
    MOZ_ASSERT(refCount > 0);
    MOZ_ASSERT(notLifoAllocated);
    if (--refCount == 0)
        fop->free_(this);
}

void
Scope::finalize(FreeOp* fop)
{
    if (data_) {
        if (is<FunctionScope>()) {
            as<FunctionScope>().bindingData().release(fop);
            fop->free_(reinterpret_cast<void*>(data_));
        } else if (is<ModuleScope>()) {
            as<ModuleScope>().bindingData().release(fop);
            fop->free_(reinterpret_cast<void*>(data_));
        } else {
            reinterpret_cast<RefCountedData*>(data_)->release(fop);
        }
        data_ = 0;
    }
}

void
Scope::dump()
{
    for (ScopeIter si(this); si; si++) {
        fprintf(stderr, "%s [%p]", ScopeKindString(si.kind()), si.scope());
        if (si.scope()->enclosing())
            fprintf(stderr, " -> ");
    }
    fprintf(stderr, "\n");
}

uint32_t
LexicalScope::firstFrameSlot() const
{
    switch (kind()) {
      case ScopeKind::Lexical:
      case ScopeKind::Catch:
        // For intra-frame scopes, find the enclosing scope's next frame slot.
        return nextFrameSlot(enclosing());
      default:
        // Otherwise start at 0.
        break;
    }
    return 0;
}

/* static */ uint32_t
LexicalScope::nextFrameSlot(Scope* scope)
{
    for (ScopeIter si(scope); si; si++) {
        switch (si.kind()) {
          case ScopeKind::Function:
            return si.scope()->as<FunctionScope>().nextFrameSlot();
          case ScopeKind::Lexical:
          case ScopeKind::Catch:
            return si.scope()->as<LexicalScope>().nextFrameSlot();
          case ScopeKind::Eval:
          case ScopeKind::StrictEval:
            return si.scope()->as<EvalScope>().nextFrameSlot();
          case ScopeKind::Module:
            return si.scope()->as<ModuleScope>().nextFrameSlot();
          case ScopeKind::With:
            continue;
          case ScopeKind::Global:
          case ScopeKind::NonSyntactic:
            return 0;
          default:
            break;
        }
    }
    MOZ_CRASH("Not an enclosing intra-frame Scope");
}

/* static */ LexicalScope*
LexicalScope::create(ExclusiveContext* cx, ScopeKind kind, Handle<BindingData*> data,
                     uint32_t firstFrameSlot, HandleScope enclosing)
{
    bool isNamedLambda = kind == ScopeKind::NamedLambda || kind == ScopeKind::StrictNamedLambda;

    MOZ_ASSERT(data, "LexicalScopes should not be created if there are no bindings.");
    MOZ_ASSERT_IF(!isNamedLambda && firstFrameSlot != 0,
                  firstFrameSlot == nextFrameSlot(enclosing));
    MOZ_ASSERT_IF(isNamedLambda, firstFrameSlot == LOCALNO_LIMIT);

    // The data that's passed in is from the frontend and is LifoAlloc'd.
    // Copy it now that we're creating a permanent VM scope.
    RootedShape envShape(cx);
    Rooted<BindingData*> copy(cx);
    auto freeOnFailure = MakeScopeExit([&copy, cx]() {
        if (copy)
            copy->release(cx->defaultFreeOp());
    });

    BindingIter bi(*data, firstFrameSlot, isNamedLambda);
    copy = CopyBindingData(cx, bi, data, sizeOfBindingData(data->length),
                           &LexicalEnvironmentObject::class_,
                           BaseShape::NOT_EXTENSIBLE | BaseShape::DELEGATE,
                           &envShape);
    if (!copy)
        return nullptr;

    Scope* scope = Scope::create(cx, kind, enclosing, envShape,
                                 reinterpret_cast<uintptr_t>(copy.get()));
    if (!scope)
        return nullptr;

    freeOnFailure.release();
    return &scope->as<LexicalScope>();
}

/* static */ Shape*
LexicalScope::getEmptyExtensibleEnvironmentShape(ExclusiveContext* cx)
{
    const Class* cls = &LexicalEnvironmentObject::class_;
    return EmptyEnvironmentShape(cx, cls, JSSLOT_FREE(cls), BaseShape::DELEGATE);
}

template <XDRMode mode>
/* static */ bool
LexicalScope::XDR(XDRState<mode>* xdr, ScopeKind kind, HandleScope enclosing,
                  MutableHandleScope scope)
{
    JSContext* cx = xdr->cx();

    Rooted<BindingData*> data(cx);
    auto freeOnLeave = MakeScopeExit([&data, cx]() {
        if (mode == XDR_DECODE && data)
            data->release(cx->defaultFreeOp());
    });

    if (!XDRSizedBindingData<LexicalScope>(xdr, scope.as<LexicalScope>(), &data))
        return false;

    if (!xdr->codeUint32(&data->constStart))
        return false;
    if (!xdr->codeUint32(&data->nextFrameSlot))
        return false;

    if (mode == XDR_DECODE) {
        scope.set(create(cx, kind, data, nextFrameSlot(enclosing), enclosing));
        if (!scope)
            return false;
    }

    return true;
}

template
/* static */ bool
LexicalScope::XDR(XDRState<XDR_ENCODE>* xdr, ScopeKind kind, HandleScope enclosing,
                  MutableHandleScope scope);

template
/* static */ bool
LexicalScope::XDR(XDRState<XDR_DECODE>* xdr, ScopeKind kind, HandleScope enclosing,
                  MutableHandleScope scope);

static const uint32_t FunctionScopeEnvShapeFlags =
    BaseShape::QUALIFIED_VAROBJ | BaseShape::DELEGATE;

/* static */ FunctionScope*
FunctionScope::create(ExclusiveContext* cx, Handle<BindingData*> bindings,
                      uint32_t firstFrameSlot, bool hasDefaults, bool needsEnvironment,
                      HandleFunction fun, HandleScope enclosing)
{
    MOZ_ASSERT_IF(firstFrameSlot != 0, firstFrameSlot == nextFrameSlot(enclosing));
    MOZ_ASSERT(fun->isTenured());

    // Note that enclosing->kind() == ScopeKind::ParameterDefaults does not
    // imply that this function scope has a parameter defaults scope. The
    // parameter defaults scope could be in an enclosing function.
    MOZ_ASSERT_IF(hasDefaults, enclosing->kind() == ScopeKind::ParameterDefaults);

    Rooted<Data*> data(cx, NewEmptyScopeData<Data>(cx, sizeof(Data)));
    if (!data)
        return nullptr;

    MOZ_ASSERT(!data->bindings);
    auto freeOnFailure = MakeScopeExit([&data, cx]() {
        if (data->bindings)
            data->bindings->release(cx->defaultFreeOp());
        js_free(data);
    });

    // The data that's passed in is from the frontend and is LifoAlloc'd.
    // Copy it now that we're creating a permanent VM scope.
    RootedShape envShape(cx);
    if (bindings) {
        BindingIter bi(*bindings, firstFrameSlot, hasDefaults);
        data->bindings = CopyBindingData(cx, bi, bindings, sizeOfBindingData(bindings->length),
                                         &CallObject::class_,
                                         FunctionScopeEnvShapeFlags, &envShape);
    } else {
        data->bindings = NewEmptyScopeBindingData<BindingData>(cx, sizeOfBindingData(1));
    }

    if (!data->bindings)
        return nullptr;

    // An environment may be needed regardless of existence of any closed over
    // bindings:
    //   - Extensible scopes (i.e., due to direct eval)
    //   - Needing a home object
    //   - Being a derived class constructor
    //   - Being a generator
    if (!envShape && needsEnvironment) {
        envShape = getEmptyEnvironmentShape(cx);
        if (!envShape)
            return nullptr;
    }

    Scope* scope = Scope::create(cx, ScopeKind::Function, enclosing, envShape,
                                 reinterpret_cast<uintptr_t>(data.get()));
    if (!scope)
        return nullptr;

    freeOnFailure.release();
    data->canonicalFunction.init(fun);
    return &scope->as<FunctionScope>();
}

/* static */ FunctionScope*
FunctionScope::clone(JSContext* cx, Handle<FunctionScope*> scope, HandleFunction fun,
                     HandleScope enclosing)
{
    MOZ_ASSERT_IF(enclosing, scope->firstFrameSlot() == nextFrameSlot(enclosing));
    MOZ_ASSERT(fun != scope->canonicalFunction());

    Rooted<Data*> dataClone(cx, NewEmptyScopeData<Data>(cx, sizeof(Data)));
    if (!dataClone)
        return nullptr;

    // The bindings are shared. Don't free them!
    auto freeOnFailure = MakeScopeExit([&dataClone]() { js_free(dataClone); });

    dataClone->bindings = scope->data().bindings;

    RootedShape envShape(cx);
    if (scope->environmentShape()) {
        envShape = scope->maybeCloneEnvironmentShape(cx);
        if (!envShape)
            return nullptr;
    }

    Scope* clone = Scope::create(cx, scope->kind(), enclosing, envShape,
                                 reinterpret_cast<uintptr_t>(dataClone.get()));
    if (!clone)
        return nullptr;

    freeOnFailure.release();
    dataClone->canonicalFunction.init(fun);
    dataClone->bindings->addRef();
    return &clone->as<FunctionScope>();
}

JSScript*
FunctionScope::script() const
{
    return canonicalFunction()->nonLazyScript();
}

/* static */ Shape*
FunctionScope::getEmptyEnvironmentShape(ExclusiveContext* cx)
{
    const Class* cls = &CallObject::class_;
    return EmptyEnvironmentShape(cx, cls, JSSLOT_FREE(cls), FunctionScopeEnvShapeFlags);
}

template <XDRMode mode>
/* static */ bool
FunctionScope::XDR(XDRState<mode>* xdr, HandleFunction fun, HandleScope enclosing,
                   MutableHandleScope scope)
{
    JSContext* cx = xdr->cx();

    Rooted<BindingData*> data(cx);
    auto freeOnLeave = MakeScopeExit([&data, cx]() {
        if (mode == XDR_DECODE && data)
            data->release(cx->defaultFreeOp());
    });

    if (!XDRSizedBindingData<FunctionScope>(xdr, scope.as<FunctionScope>(), &data))
        return false;

    uint8_t hasDefaults;
    uint8_t needsEnvironment;
    if (mode == XDR_ENCODE) {
        hasDefaults = fun->nonLazyScript()->hasDefaults();
        needsEnvironment = scope->hasEnvironment();
    }
    if (!xdr->codeUint8(&hasDefaults))
        return false;
    if (!xdr->codeUint8(&needsEnvironment))
        return false;
    if (!xdr->codeUint16(&data->nonPositionalFormalStart))
        return false;
    if (!xdr->codeUint16(&data->varStart))
        return false;
    if (!xdr->codeUint32(&data->nextFrameSlot))
        return false;

    if (mode == XDR_DECODE) {
        if (!data->length) {
            MOZ_ASSERT(!data->nonPositionalFormalStart);
            MOZ_ASSERT(!data->varStart);
            MOZ_ASSERT(!data->nextFrameSlot);
            data->release(cx->defaultFreeOp());
            data = nullptr;
        }

        scope.set(create(cx, data, nextFrameSlot(enclosing), hasDefaults,
                         needsEnvironment, fun, enclosing));

        if (!scope)
            return false;
    }

    return true;
}

template
/* static */ bool
FunctionScope::XDR(XDRState<XDR_ENCODE>* xdr, HandleFunction fun, HandleScope enclosing,
                   MutableHandleScope scope);

template
/* static */ bool
FunctionScope::XDR(XDRState<XDR_DECODE>* xdr, HandleFunction fun, HandleScope enclosing,
                   MutableHandleScope scope);

/* static */ GlobalScope*
GlobalScope::create(ExclusiveContext* cx, ScopeKind kind, Handle<BindingData*> data)
{
    // The data that's passed in is from the frontend and is LifoAlloc'd.
    // Copy it now that we're creating a permanent VM scope.
    Rooted<BindingData*> copy(cx);
    auto freeOnFailure = MakeScopeExit([&copy, cx]() {
        if (copy)
            copy->release(cx->defaultFreeOp());
    });

    if (data) {
        // The global scope has no environment shape. Its environment is the
        // global lexical scope and the global object or non-syntactic objects
        // created by embedding, all of which are not only extensible but may
        // have names on them deleted.
        copy = CopyBindingData(cx, data, sizeOfBindingData(data->length));
    } else {
        copy = NewEmptyScopeBindingData<BindingData>(cx, sizeOfBindingData(1));
    }

    if (!copy)
        return nullptr;

    Scope* scope = Scope::create(cx, kind, nullptr, nullptr,
                                 reinterpret_cast<uintptr_t>(copy.get()));
    if (!scope)
        return nullptr;

    freeOnFailure.release();
    return &scope->as<GlobalScope>();
}

/* static */ GlobalScope*
GlobalScope::clone(JSContext* cx, Handle<GlobalScope*> scope, ScopeKind kind)
{
    Scope* clone = Scope::create(cx, kind, nullptr, nullptr, scope->data_);
    if (!clone)
        return nullptr;

    scope->bindingData().addRef();
    return &clone->as<GlobalScope>();
}

template <XDRMode mode>
/* static */ bool
GlobalScope::XDR(XDRState<mode>* xdr, ScopeKind kind, MutableHandleScope scope)
{
    MOZ_ASSERT((mode == XDR_DECODE) == !scope);

    JSContext* cx = xdr->cx();

    Rooted<BindingData*> data(cx);
    auto freeOnLeave = MakeScopeExit([&data, cx]() {
        if (mode == XDR_DECODE && data)
            data->release(cx->defaultFreeOp());
    });

    if (!XDRSizedBindingData<GlobalScope>(xdr, scope.as<GlobalScope>(), &data))
        return false;

    if (!xdr->codeUint32(&data->letStart))
        return false;
    if (!xdr->codeUint32(&data->constStart))
        return false;

    if (mode == XDR_DECODE) {
        if (!data->length) {
            MOZ_ASSERT(!data->letStart);
            MOZ_ASSERT(!data->constStart);
            data->release(cx->defaultFreeOp());
            data = nullptr;
        }

        scope.set(create(cx, kind, data));

        if (!scope)
            return false;
    }

    return true;
}

template
/* static */ bool
GlobalScope::XDR(XDRState<XDR_ENCODE>* xdr, ScopeKind kind, MutableHandleScope scope);

template
/* static */ bool
GlobalScope::XDR(XDRState<XDR_DECODE>* xdr, ScopeKind kind, MutableHandleScope scope);

/* static */ WithScope*
WithScope::create(ExclusiveContext* cx, HandleScope enclosing)
{
    Scope* scope = Scope::create(cx, ScopeKind::With, enclosing, nullptr, 0);
    return static_cast<WithScope*>(scope);
}

static const uint32_t EvalScopeEnvShapeFlags =
    BaseShape::QUALIFIED_VAROBJ | BaseShape::DELEGATE;

/* static */ EvalScope*
EvalScope::create(ExclusiveContext* cx, ScopeKind scopeKind, Handle<BindingData*> data,
                  HandleScope enclosing)
{
    // The data that's passed in is from the frontend and is LifoAlloc'd.
    // Copy it now that we're creating a permanent VM scope.
    RootedShape envShape(cx);
    Rooted<BindingData*> copy(cx);
    auto freeOnFailure = MakeScopeExit([&copy, cx]() {
        if (copy)
            copy->release(cx->defaultFreeOp());
    });

    if (data) {
        if (scopeKind == ScopeKind::StrictEval) {
            BindingIter bi(*data, true);
            copy = CopyBindingData(cx, bi, data, sizeOfBindingData(data->length),
                                   &CallObject::class_,
                                   EvalScopeEnvShapeFlags, &envShape);
        } else {
            copy = CopyBindingData(cx, data, sizeOfBindingData(data->length));
        }
    } else {
        copy = NewEmptyScopeBindingData<BindingData>(cx, sizeOfBindingData(1));
    }

    if (!copy)
        return nullptr;

    // Strict eval and direct eval in parameter defaults always get their own
    // var environment even if there are no bindings.
    if (!envShape && (scopeKind == ScopeKind::StrictEval ||
                      enclosing->kind() == ScopeKind::ParameterDefaults))
    {
        envShape = getEmptyEnvironmentShape(cx);
        if (!envShape)
            return nullptr;
    }

    Scope* scope = Scope::create(cx, scopeKind, enclosing, envShape,
                                 reinterpret_cast<uintptr_t>(copy.get()));
    if (!scope)
        return nullptr;

    freeOnFailure.release();
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

/* static */ Shape*
EvalScope::getEmptyEnvironmentShape(ExclusiveContext* cx)
{
    const Class* cls = &CallObject::class_;
    return EmptyEnvironmentShape(cx, cls, JSSLOT_FREE(cls), EvalScopeEnvShapeFlags);
}

template <XDRMode mode>
/* static */ bool
EvalScope::XDR(XDRState<mode>* xdr, ScopeKind kind, HandleScope enclosing,
               MutableHandleScope scope)
{
    JSContext* cx = xdr->cx();

    Rooted<BindingData*> data(cx);
    auto freeOnLeave = MakeScopeExit([&data, cx]() {
        if (mode == XDR_DECODE && data)
            data->release(cx->defaultFreeOp());
    });

    if (!XDRSizedBindingData<EvalScope>(xdr, scope.as<EvalScope>(), &data))
        return false;

    if (!xdr->codeUint32(&data->nextFrameSlot))
        return false;

    if (mode == XDR_DECODE) {
        if (!data->length) {
            MOZ_ASSERT(!data->nextFrameSlot);
            data->release(cx->defaultFreeOp());
            data = nullptr;
        }

        scope.set(create(cx, kind, data, enclosing));

        if (!scope)
            return false;
    }

    return true;
}

template
/* static */ bool
EvalScope::XDR(XDRState<XDR_ENCODE>* xdr, ScopeKind kind, HandleScope enclosing,
               MutableHandleScope scope);

template
/* static */ bool
EvalScope::XDR(XDRState<XDR_DECODE>* xdr, ScopeKind kind, HandleScope enclosing,
               MutableHandleScope scope);

static const uint32_t ModuleScopeEnvShapeFlags =
    BaseShape::NOT_EXTENSIBLE | BaseShape::QUALIFIED_VAROBJ | BaseShape::DELEGATE;

/* static */ ModuleScope*
ModuleScope::create(ExclusiveContext* cx, Handle<BindingData*> bindings,
                    HandleModuleObject module, HandleScope enclosing)
{
    MOZ_ASSERT(enclosing->is<GlobalScope>());

    Rooted<Data*> data(cx, NewEmptyScopeData<Data>(cx, sizeof(Data)));
    if (!data)
        return nullptr;

    MOZ_ASSERT(!data->bindings);
    auto freeOnFailure = MakeScopeExit([&data, cx]() {
        if (data->bindings)
            data->bindings->release(cx->defaultFreeOp());
        js_free(data);
    });

    // The data that's passed in is from the frontend and is LifoAlloc'd.
    // Copy it now that we're creating a permanent VM scope.
    RootedShape envShape(cx);
    if (bindings) {
        BindingIter bi(*bindings);
        data->bindings = CopyBindingData(cx, bi, bindings, sizeOfBindingData(bindings->length),
                                         &ModuleEnvironmentObject::class_,
                                         ModuleScopeEnvShapeFlags, &envShape);

    } else {
        data->bindings = NewEmptyScopeBindingData<BindingData>(cx, sizeOfBindingData(1));
    }

    if (!data->bindings)
        return nullptr;

    // Find the last var frame slot.
    data->bindings->varFrameSlotEnd = data->bindings->nextFrameSlot;
    for (BindingIter bi(*data->bindings); bi; bi++) {
        if (bi.location().kind() == BindingLocation::Kind::Frame &&
            BindingKindIsLexical(bi.kind()))
        {
            data->bindings->varFrameSlotEnd = bi.location().slot();
            break;
        }
    }

    // Modules always need an environment object for now.
    if (!envShape) {
        envShape = getEmptyEnvironmentShape(cx);
        if (!envShape)
            return nullptr;
    }

    Scope* scope = Scope::create(cx, ScopeKind::Module, enclosing, envShape,
                                 reinterpret_cast<uintptr_t>(data.get()));
    if (!scope)
        return nullptr;

    freeOnFailure.release();
    data->module.init(module);
    return &scope->as<ModuleScope>();
}

/* static */ Shape*
ModuleScope::getEmptyEnvironmentShape(ExclusiveContext* cx)
{
    const Class* cls = &ModuleEnvironmentObject::class_;
    return EmptyEnvironmentShape(cx, cls, JSSLOT_FREE(cls), ModuleScopeEnvShapeFlags);
}

JSScript*
ModuleScope::script() const
{
    return module()->script();
}

ScopeIter::ScopeIter(JSScript* script)
  : scope_(script->bodyScope())
{ }

bool
ScopeIter::hasSyntacticEnvironment() const
{
    return scope()->hasEnvironment() && scope()->kind() != ScopeKind::NonSyntactic;
}

BindingIter::BindingIter(Scope* scope)
{
    switch (scope->kind()) {
      case ScopeKind::ParameterDefaults:
      case ScopeKind::Lexical:
      case ScopeKind::Catch:
        init(scope->as<LexicalScope>().bindingData(),
             scope->as<LexicalScope>().firstFrameSlot(), 0);
        break;
      case ScopeKind::NamedLambda:
      case ScopeKind::StrictNamedLambda:
        init(scope->as<LexicalScope>().bindingData(), LOCALNO_LIMIT, IsNamedLambda);
        break;
      case ScopeKind::With:
        // With scopes do not have bindings.
        index_ = length_ = 0;
        MOZ_ASSERT(done());
        break;
      case ScopeKind::Function: {
        uint8_t ignoreFlags = IgnoreDestructuredFormalParameters;
        if (scope->as<FunctionScope>().canonicalFunction()->nonLazyScript()->hasDefaults())
            ignoreFlags |= IgnorePositionalFormalParameters;
        init(scope->as<FunctionScope>().bindingData(),
             scope->as<FunctionScope>().firstFrameSlot(),
             ignoreFlags);
        break;
      }
      case ScopeKind::Eval:
      case ScopeKind::StrictEval:
        init(scope->as<EvalScope>().bindingData(), scope->kind() == ScopeKind::StrictEval);
        break;
      case ScopeKind::Global:
      case ScopeKind::NonSyntactic:
        init(scope->as<GlobalScope>().bindingData());
        break;
      case ScopeKind::Module:
        init(scope->as<ModuleScope>().bindingData());
        break;
    }
}

BindingIter::BindingIter(JSScript* script)
  : BindingIter(script->bodyScope())
{ }

void
BindingIter::init(LexicalScope::BindingData& data, uint32_t firstFrameSlot, uint8_t flags)
{
    // Named lambda scopes can only have environment slots. If the callee
    // isn't closed over, it is accessed via JSOP_CALLEE.
    if (flags & IsNamedLambda) {
        // Named lambda binding is weird. Normal BindingKind ordering rules
        // don't apply.
        init(0, 0, 0, 0, 0,
             CanHaveEnvironmentSlots | flags,
             firstFrameSlot, JSSLOT_FREE(&LexicalEnvironmentObject::class_),
             data.names, data.length);
    } else {
        //            imports - [0, 0)
        // positional formals - [0, 0)
        //      other formals - [0, 0)
        //               vars - [0, 0)
        //               lets - [0, data.constStart)
        //             consts - [data.constStart, data.length)
        init(0, 0, 0, 0, data.constStart,
             CanHaveFrameSlots | CanHaveEnvironmentSlots | flags,
             firstFrameSlot, JSSLOT_FREE(&LexicalEnvironmentObject::class_),
             data.names, data.length);
    }
}

void
BindingIter::init(FunctionScope::BindingData& data, uint32_t firstFrameSlot, uint8_t flags)
{
    //            imports - [0, 0)
    // positional formals - [0, data.nonPositionalFormalStart)
    //      other formals - [data.nonPositionalParamStart, data.varStart)
    //               vars - [data.varStart, data.length)
    //               lets - [data.length, data.length)
    //             consts - [data.length, data.length)
    init(0, data.nonPositionalFormalStart, data.varStart, data.length, data.length,
         CanHaveArgumentSlots | CanHaveFrameSlots | CanHaveEnvironmentSlots | flags,
         firstFrameSlot, JSSLOT_FREE(&CallObject::class_),
         data.names, data.length);
}

void
BindingIter::init(GlobalScope::BindingData& data)
{
    //            imports - [0, 0)
    // positional formals - [0, 0)
    //      other formals - [0, 0)
    //               vars - [0, data.letStart)
    //               lets - [data.letStart, data.constStart)
    //             consts - [data.constStart, data.length)
    init(0, 0, 0, data.letStart, data.constStart,
         CannotHaveSlots,
         UINT32_MAX, UINT32_MAX,
         data.names, data.length);
}

void
BindingIter::init(EvalScope::BindingData& data, bool strict)
{
    uint32_t flags;
    uint32_t firstFrameSlot;
    uint32_t firstEnvironmentSlot;
    if (strict) {
        flags = CanHaveFrameSlots | CanHaveEnvironmentSlots;
        firstFrameSlot = 0;
        firstEnvironmentSlot = JSSLOT_FREE(&CallObject::class_);
    } else {
        flags = CannotHaveSlots;
        firstFrameSlot = UINT32_MAX;
        firstEnvironmentSlot = UINT32_MAX;
    }

    //            imports - [0, 0)
    // positional formals - [0, 0)
    //      other formals - [0, 0)
    //               vars - [0, data.length)
    //               lets - [data.length, data.length)
    //             consts - [data.length, data.length)
    init(0, 0, 0, data.length, data.length,
         flags, firstFrameSlot, firstEnvironmentSlot,
         data.names, data.length);
}

void
BindingIter::init(ModuleScope::BindingData& data)
{
    //            imports - [0, data.varStart)
    // positional formals - [data.varStart, data.varStart)
    //      other formals - [data.varStart, data.varStart)
    //               vars - [data.varStart, data.letStart)
    //               lets - [data.letStart, data.constStart)
    //             consts - [data.constStart, data.length)
    init(data.varStart, data.varStart, data.varStart, data.letStart, data.constStart,
         CanHaveFrameSlots | CanHaveEnvironmentSlots,
         0, JSSLOT_FREE(&ModuleEnvironmentObject::class_),
         data.names, data.length);
}

PositionalFormalParameterIter::PositionalFormalParameterIter(JSScript* script)
  : BindingIter(script),
    hasDefaults_(script->hasDefaults())
{
    // Reinit with flags = 0, i.e., iterate over all positional parameters.
    if (script->bodyScope()->is<FunctionScope>())
        init(script->bodyScope()->as<FunctionScope>().bindingData(), 0, /* flags = */ 0);
    settle();
}

void
js::DumpBindings(JSContext* cx, Scope* scope) {
    for (BindingIter bi(scope); bi; bi++) {
        JSAutoByteString bytes;
        if (!AtomToPrintableString(cx, bi.name(), &bytes))
            return;
        fprintf(stderr, "%s %s ", BindingKindString(bi.kind()), bytes.ptr());
        switch (bi.location().kind()) {
          case BindingLocation::Kind::Global:
            fprintf(stderr, "global\n");
            break;
          case BindingLocation::Kind::Argument:
            fprintf(stderr, "arg slot %u\n", bi.location().argumentSlot());
            break;
          case BindingLocation::Kind::Frame:
            fprintf(stderr, "frame slot %u\n", bi.location().slot());
            break;
          case BindingLocation::Kind::Environment:
            fprintf(stderr, "env slot %u\n", bi.location().slot());
            break;
          case BindingLocation::Kind::NamedLambdaCallee:
            fprintf(stderr, "named lambda callee\n");
            break;
          case BindingLocation::Kind::Import:
            fprintf(stderr, "import\n");
            break;
        }
    }

    if (scope->is<FunctionScope>()) {
        JSScript* script =  scope->as<FunctionScope>().canonicalFunction()->nonLazyScript();
        if (script->hasDefaults()) {
            for (PositionalFormalParameterIter fi(script); fi; fi++) {
                JSAutoByteString bytes;
                if (!AtomToPrintableString(cx, fi.name(), &bytes))
                    return;
                fprintf(stderr, "%s %s (in defaults scope)\n",
                        BindingKindString(fi.kind()), bytes.ptr());
            }
        }
    }
}

size_t
LexicalScope::sizeOfData(mozilla::MallocSizeOf mallocSizeOf) const
{
    return mallocSizeOf(&bindingData());
}

size_t
FunctionScope::sizeOfData(mozilla::MallocSizeOf mallocSizeOf) const
{
    return mallocSizeOf(&data()) + mallocSizeOf(&bindingData());
}

size_t
GlobalScope::sizeOfData(mozilla::MallocSizeOf mallocSizeOf) const
{
    return mallocSizeOf(&bindingData());
}

size_t
EvalScope::sizeOfData(mozilla::MallocSizeOf mallocSizeOf) const
{
    return mallocSizeOf(&bindingData());
}

size_t
ModuleScope::sizeOfData(mozilla::MallocSizeOf mallocSizeOf) const
{
    return mallocSizeOf(&bindingData());
}

JS::ubi::Node::Size
JS::ubi::Concrete<Scope>::size(mozilla::MallocSizeOf mallocSizeOf) const
{
    Size size = js::gc::Arena::thingSize(get().asTenured().getAllocKind());

    if (get().is<LexicalScope>())
        size += get().as<LexicalScope>().sizeOfData(mallocSizeOf);
    else if (get().is<FunctionScope>())
        size += get().as<FunctionScope>().sizeOfData(mallocSizeOf);
    else if (get().is<GlobalScope>())
        size += get().as<GlobalScope>().sizeOfData(mallocSizeOf);
    else if (get().is<EvalScope>())
        size += get().as<EvalScope>().sizeOfData(mallocSizeOf);
    else if (get().is<ModuleScope>())
        size += get().as<ModuleScope>().sizeOfData(mallocSizeOf);

    return size;
}

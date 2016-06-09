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
CopyScopeData(ExclusiveContext* cx, ScopeData* data, size_t dataSize)
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
CopyScopeData(ExclusiveContext* cx, BindingIter& bi, const Class* cls,
              uint32_t baseShapeFlags, ScopeData* data, size_t dataSize)
{
    Shape* envShape = CreateEnvironmentShape(cx, bi, cls, bi.nextEnvironmentSlot(),
                                             baseShapeFlags);
    if (!envShape)
        return nullptr;

    ScopeData* copy = CopyScopeData(cx, data, dataSize);
    if (copy)
        copy->environmentShape.init(envShape);
    return copy;
}

template <typename ScopeData>
static ScopeData*
CopyFrameScopeData(ExclusiveContext* cx, BindingIter& bi, const Class* cls,
                   uint32_t baseShapeFlags, ScopeData* data, size_t dataSize)
{
    // Copy a fresh BindingIter for use below.
    BindingIter freshBi(bi);

    // Iterate through all bindings. This counts the number of environment
    // slots needed and computes the maximum frame slot.
    while (bi)
        bi++;
    data->nextFrameSlot = bi.nextFrameSlot();

    return CopyScopeData(cx, freshBi, cls, baseShapeFlags, data, dataSize);
}

template <typename ScopeData>
static ScopeData*
NewEmptyScopeData(ExclusiveContext* cx, size_t dataSize)
{
    uint8_t* bytes = cx->zone()->pod_calloc<uint8_t>(dataSize);
    return reinterpret_cast<ScopeData*>(bytes);
}

/* static */ Scope*
Scope::create(ExclusiveContext* cx, ScopeKind kind, Scope* enclosing, uintptr_t data)
{
    Scope* scope = Allocate<Scope>(cx);
    if (scope)
        new (scope) Scope(kind, enclosing, data);
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

Scope*
Scope::clone(JSContext* cx, Scope* enclosing)
{
    MOZ_ASSERT(!is<FunctionScope>() && !is<GlobalScope>(),
               "FunctionScopes and GlobalScopes should use the class-specific clone.");

    if (is<LexicalScope>()) {
        LexicalScope& self = as<LexicalScope>();
        return LexicalScope::create(cx, self.kind(), &self.data(), self.computeFirstFrameSlot(),
                                    enclosing);
    }

    if (is<EvalScope>()) {
        EvalScope& self = as<EvalScope>();
        return EvalScope::create(cx, self.kind(), &self.data(), enclosing);
    }

    MOZ_ASSERT(is<WithScope>());
    return WithScope::create(cx, enclosing);
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
LexicalScope::create(ExclusiveContext* cx, ScopeKind kind, Data* data,
                     uint32_t firstFrameSlot, Scope* enclosing)
{
    MOZ_ASSERT(data, "LexicalScopes should not be created if there are no bindings.");
    MOZ_ASSERT(firstFrameSlot == computeNextFrameSlot(enclosing));

    // The data that's passed in is from the frontend and is LifoAlloc'd or is
    // from Scope::copy. Copy it now that we're creating a permanent VM scope.
    Data* copy;
    BindingIter bi(*data, firstFrameSlot);
    copy = CopyFrameScopeData(cx, bi,
                              &ClonedBlockObject::class_,
                              BaseShape::NOT_EXTENSIBLE | BaseShape::DELEGATE,
                              data, sizeOfData(data->length));
    if (!copy)
        return nullptr;

    Scope* scope = Scope::create(cx, kind, enclosing, reinterpret_cast<uintptr_t>(copy));
    if (!scope)
        js_free(copy);
    MOZ_ASSERT(static_cast<LexicalScope*>(scope)->is<LexicalScope>());
    return static_cast<LexicalScope*>(scope);
}

/* static */ FunctionScope*
FunctionScope::create(ExclusiveContext* cx, Data* data, uint32_t firstFrameSlot,
                      JSFunction* fun, Scope* enclosing)
{
    MOZ_ASSERT_IF(enclosing, firstFrameSlot == computeNextFrameSlot(enclosing));
    MOZ_ASSERT(fun->isTenured());

    // The data that's passed in is from the frontend and is LifoAlloc'd or is
    // from Scope::copy. Copy it now that we're creating a permanent VM scope.
    Data* copy;
    if (data) {
        BindingIter bi(*data, firstFrameSlot);
        copy = CopyFrameScopeData(cx, bi,
                                  &CallObject::class_,
                                  BaseShape::QUALIFIED_VAROBJ | BaseShape::DELEGATE,
                                  data, sizeOfData(data->length));
    } else {
        copy = NewEmptyScopeData<Data>(cx, sizeOfData(1));
    }

    if (!copy)
        return nullptr;

    copy->canonicalFunction.init(fun);

    Scope* scope = Scope::create(cx, ScopeKind::Function, enclosing,
                                 reinterpret_cast<uintptr_t>(copy));
    if (!scope)
        js_free(copy);
    return static_cast<FunctionScope*>(scope);
}

FunctionScope*
FunctionScope::clone(JSContext* cx, JSFunction* fun, Scope* enclosing)
{
    MOZ_ASSERT(fun != canonicalFunction());
    return create(cx, &data(), computeFirstFrameSlot(), fun, enclosing);
}

JSScript*
FunctionScope::script() const
{
    return canonicalFunction()->nonLazyScript();
}

/* static */ GlobalScope*
GlobalScope::create(ExclusiveContext* cx, ScopeKind kind, Data* data)
{
    // The data that's passed in is from the frontend and is LifoAlloc'd or is
    // from Scope::copy. Copy it now that we're creating a permanent VM scope.
    Data* copy;
    if (data) {
        // The global scope has no environment shape. Its environment is the
        // global lexical scope and the global object or non-syntactic objects
        // created by embedding, all of which are not only extensible but may
        // have names on them deleted.
        copy = CopyScopeData(cx, data, sizeOfData(data->length));
    } else {
        copy = NewEmptyScopeData<Data>(cx, sizeOfData(1));
    }

    if (!copy)
        return nullptr;

    Scope* scope = Scope::create(cx, kind, nullptr, reinterpret_cast<uintptr_t>(copy));
    if (!scope)
        js_free(copy);
    MOZ_ASSERT(static_cast<GlobalScope*>(scope)->is<GlobalScope>());
    return static_cast<GlobalScope*>(scope);
}

GlobalScope*
GlobalScope::clone(JSContext* cx, ScopeKind kind)
{
    return create(cx, kind, &data());
}

/* static */ WithScope*
WithScope::create(ExclusiveContext* cx, Scope* enclosing)
{
    Scope* scope = Scope::create(cx, ScopeKind::With, enclosing, 0);
    return static_cast<WithScope*>(scope);
}

/* static */ EvalScope*
EvalScope::create(ExclusiveContext* cx, ScopeKind scopeKind, Data* data, Scope* enclosing)
{
    MOZ_ASSERT(scopeKind == ScopeKind::Eval || scopeKind == ScopeKind::StrictEval);

    // The data that's passed in is from the frontend and is LifoAlloc'd or is
    // from Scope::copy. Copy it now that we're creating a permanent VM scope.
    Data* copy;
    if (data) {
        if (scopeKind == ScopeKind::StrictEval) {
            BindingIter bi(*data, true);
            copy = CopyFrameScopeData(cx, bi,
                                      &CallObject::class_,
                                      BaseShape::QUALIFIED_VAROBJ | BaseShape::DELEGATE,
                                      data, sizeOfData(data->length));
        } else {
            copy = CopyScopeData(cx, data, sizeOfData(data->length));
        }
    } else {
        copy = NewEmptyScopeData<Data>(cx, sizeOfData(1));
    }

    if (!copy)
        return nullptr;

    Scope* scope = Scope::create(cx, scopeKind, enclosing, reinterpret_cast<uintptr_t>(copy));
    if (!scope)
        js_free(copy);
    return static_cast<EvalScope*>(scope);
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
        return !!scope()->as<FunctionScope>().environmentShape();

      case ScopeKind::ParameterDefaults:
      case ScopeKind::Lexical:
      case ScopeKind::Catch:
        return !!scope()->as<LexicalScope>().environmentShape();

      case ScopeKind::StrictEval:
        return !!scope()->as<EvalScope>().environmentShape();

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

Shape*
ScopeIter::environmentShape() const
{
    MOZ_ASSERT(hasSyntacticEnvironment());

    switch (scope()->kind()) {
      case ScopeKind::Function:
        return scope()->as<FunctionScope>().environmentShape();

      case ScopeKind::Lexical:
      case ScopeKind::Catch:
        return scope()->as<LexicalScope>().environmentShape();

      case ScopeKind::Eval:
        return scope()->as<EvalScope>().environmentShape();

      default:
        MOZ_CRASH("Scope does not have a syntactic environment");
    }
}

void
Scope::finalize(FreeOp* fop)
{
    if (data_) {
        fop->free_(reinterpret_cast<void*>(data_));
        data_ = 0;
    }
}

BindingIter::BindingIter(JSScript* script)
  : BindingIter(script->bodyScope())
{ }

void
BindingIter::init(LexicalScope::Data& data, uint32_t firstFrameSlot)
{
    init(0, 0, 0, data.constStart,
         CanHaveFrameSlots | CanHaveEnvironmentSlots,
         firstFrameSlot, JSSLOT_FREE(&ClonedBlockObject::class_),
         data.names, data.length);
}

void
BindingIter::init(FunctionScope::Data& data, uint32_t firstFrameSlot)
{
    init(data.nonSimpleFormalStart, data.varStart, data.length, 0,
         CanHaveArgumentSlots | CanHaveFrameSlots | CanHaveEnvironmentSlots,
         firstFrameSlot, JSSLOT_FREE(&CallObject::class_),
         data.names, data.length);
}

void
BindingIter::init(GlobalScope::Data& data)
{
    init(0, 0, data.letStart, data.constStart,
         CannotHaveSlots,
         UINT32_MAX, UINT32_MAX,
         data.names, data.length);
}

void
BindingIter::init(EvalScope::Data& data, bool strict)
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

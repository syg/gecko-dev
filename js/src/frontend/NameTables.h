/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef frontend_NameTables_h
#define frontend_NameTables_h

#include "ds/InlineTable.h"
#include "js/Vector.h"
#include "vm/Stack.h"

namespace js {
namespace frontend {

enum class DeclarationKind : uint8_t
{
    SimpleFormalParameter,
    FormalParameter,
    Var,
    Let,
    Const,
    Import,
    BodyLevelFunction,
    LexicalFunction,
    CatchParameter
};

static inline BindingKind
DeclarationKindToBindingKind(DeclarationKind kind)
{
    switch (kind) {
      case DeclarationKind::SimpleFormalParameter:
      case DeclarationKind::FormalParameter:
        return BindingKind::FormalParameter;

      case DeclarationKind::Var:
      case DeclarationKind::BodyLevelFunction:
        return BindingKind::Var;

      case DeclarationKind::Let:
      case DeclarationKind::Import:
      case DeclarationKind::LexicalFunction:
      case DeclarationKind::CatchParameter:
        return BindingKind::Let;

      case DeclarationKind::Const:
        return BindingKind::Const;

      default:
        MOZ_CRASH("Bad DeclarationKind");
    }
}

static inline bool
DeclarationKindIsLexical(DeclarationKind kind)
{
    return BindingKindIsLexical(DeclarationKindToBindingKind(kind));
}

// Used in Parser to track declared names.
class DeclaredNameInfo
{
    DeclarationKind kind_;

    // If the declared name is a binding, whether the binding is closed
    // over. Its value is meaningless if the declared name is not a binding
    // (i.e., a 'var' declared name in a non-var scope).
    bool closedOver_;

  public:
    // Default constructor for InlineMap.
    DeclaredNameInfo() = default;

    explicit DeclaredNameInfo(DeclarationKind kind)
      : kind_(kind),
        closedOver_(false)
    { }

    DeclarationKind kind() const {
        return kind_;
    }

    void setClosedOver() {
        closedOver_ = true;
    }

    bool closedOver() const {
        return closedOver_;
    }
};

// Used in BytecodeEmitter to map names to locations.
class NameLocation
{
  public:
    enum class Kind : uint8_t
    {
        // Cannot statically determine where the name lives. Needs to walk the
        // environment chain to search for the name.
        Dynamic,

        // The name lives on the global. Search for the name on the global scope.
        Global,

        // Special mode used only when emitting self-hosted scripts. See
        // BytecodeEmitter::lookupName.
        Intrinsic,

        // In a named lambda, the name is the callee itself.
        NamedLambdaCallee,

        // The name is a simple formal parameter name and can be retrieved
        // directly from the stack using slot_.
        ArgumentSlot,

        // The name is not closed over and lives on the frame in slot_.
        FrameSlot,

        // The name is closed over and lives on an environment hops_ away in slot_.
        EnvironmentCoordinate,
    };

  private:
    // Where the name lives.
    Kind kind_;

    // If the name is not a dynamic lookup, the kind of the binding.
    BindingKind bindingKind_;

    // If the name is closed over and accessed via EnvironmentCoordinate, the
    // number of dynamic environments to skip.
    //
    // Otherwise UINT8_MAX.
    uint8_t hops_;

    // If the name lives on the frame, the slot frame.
    //
    // If the name is closed over and accessed via EnvironmentCoordinate, the
    // slot on the environment.
    //
    // Otherwise LOCALNO_LIMIT/SCOPECOORD_SLOT_LIMIT.
    uint32_t slot_ : SCOPECOORD_SLOT_BITS;

    NameLocation(Kind kind, BindingKind bindingKind,
                 uint8_t hops = UINT8_MAX, uint32_t slot = SCOPECOORD_SLOT_LIMIT)
      : kind_(kind),
        bindingKind_(bindingKind),
        hops_(hops),
        slot_(slot)
    { }

  public:
    // Default constructor for InlineMap.
    NameLocation() = default;

    static NameLocation Dynamic() {
        return NameLocation();
    }

    static NameLocation Global(BindingKind bindKind) {
        MOZ_ASSERT(bindKind != BindingKind::FormalParameter);
        return NameLocation(Kind::Global, bindKind);
    }

    static NameLocation Intrinsic() {
        return NameLocation(Kind::Intrinsic, BindingKind::Var);
    }

    static NameLocation NamedLambdaCallee() {
        return NameLocation(Kind::NamedLambdaCallee, BindingKind::Const);
    }

    static NameLocation ArgumentSlot(uint16_t slot) {
        return NameLocation(Kind::ArgumentSlot, BindingKind::FormalParameter, 0, slot);
    }

    static NameLocation FrameSlot(BindingKind bindKind, uint32_t slot) {
        return NameLocation(Kind::FrameSlot, bindKind, 0, slot);
    }

    static NameLocation EnvironmentCoordinate(BindingKind bindKind, uint8_t hops, uint32_t slot) {
        return NameLocation(Kind::EnvironmentCoordinate, bindKind, hops, slot);
    }

    static NameLocation fromBinding(BindingKind bindKind, const BindingLocation& bl) {
        switch (bl.kind()) {
          case BindingLocation::Kind::Global:
            return Global(bindKind);
          case BindingLocation::Kind::Argument:
            return ArgumentSlot(bl.argumentSlot());
          case BindingLocation::Kind::Frame:
            return FrameSlot(bindKind, bl.slot());
          case BindingLocation::Kind::Environment:
            return EnvironmentCoordinate(bindKind, 0, bl.slot());
          default:
            MOZ_CRASH("Bad BindingLocation kind");
        }
    }

    bool operator==(const NameLocation& other) const {
        return kind_ == other.kind_ && bindingKind_ == other.bindingKind_ &&
               hops_ == other.hops_ && slot_ == other.slot_;
    }

    bool operator!=(const NameLocation& other) const {
        return !operator==(other);
    }

    Kind kind() const {
        return kind_;
    }

    uint16_t argumentSlot() const {
        MOZ_ASSERT(kind_ == Kind::ArgumentSlot);
        return mozilla::AssertedCast<uint16_t>(slot_);
    }

    uint32_t frameSlot() const {
        MOZ_ASSERT(kind_ == Kind::FrameSlot);
        return slot_;
    }

    NameLocation addHops(uint8_t more) {
        MOZ_ASSERT(hops_ < SCOPECOORD_HOPS_LIMIT - more);
        MOZ_ASSERT(kind_ == Kind::EnvironmentCoordinate);
        return NameLocation(kind_, bindingKind_, hops_ + more, slot_);
    }

    ScopeCoordinate scopeCoordinate() const {
        MOZ_ASSERT(kind_ == Kind::EnvironmentCoordinate);
        ScopeCoordinate coord;
        coord.setHops(hops_);
        coord.setSlot(slot_);
        return coord;
    }

    BindingKind bindingKind() const {
        MOZ_ASSERT(kind_ != Kind::Dynamic);
        return bindingKind_;
    }

    bool isLexical() const {
        return BindingKindIsLexical(bindingKind());
    }

    bool isConst() const {
        return bindingKind() == BindingKind::Const;
    }

    bool isGlobal() const {
        return kind_ == Kind::Global;
    }

    bool hasKnownSlot() const {
        return kind_ != Kind::Dynamic && kind_ != Kind::Global && kind_ != Kind::Intrinsic;
    }
};

static_assert(LOCALNO_BITS == SCOPECOORD_SLOT_BITS,
              "Frame and environment slots must be same sized.");

template <typename Wrapped>
struct RecyclableAtomMapValueWrapper
{
    union {
        Wrapped wrapped;
        uint64_t dummy;
    };

    static void assertInvariant() {
        static_assert(sizeof(Wrapped) <= sizeof(uint64_t),
                      "Can only recycle atom maps with values smaller than uint64");
    }

    RecyclableAtomMapValueWrapper() {
        assertInvariant();
    }

    MOZ_IMPLICIT RecyclableAtomMapValueWrapper(Wrapped w)
      : wrapped(w)
    {
        assertInvariant();
    }

    MOZ_IMPLICIT operator Wrapped&() {
        return wrapped;
    }

    MOZ_IMPLICIT operator Wrapped&() const {
        return wrapped;
    }

    Wrapped* operator->() {
        return &wrapped;
    }

    const Wrapped* operator->() const {
        return &wrapped;
    }
};

#define RECYCLABLE_NAME_MAP_TYPE(MapValue)              \
    InlineMap<JSAtom*,                                  \
              RecyclableAtomMapValueWrapper<MapValue>,  \
              24,                                       \
              DefaultHasher<JSAtom*>,                   \
              SystemAllocPolicy>

using DeclaredNameMap = RECYCLABLE_NAME_MAP_TYPE(DeclaredNameInfo);
using CheckTDZMap = RECYCLABLE_NAME_MAP_TYPE(MaybeCheckTDZ);
using NameLocationMap = RECYCLABLE_NAME_MAP_TYPE(NameLocation);
using AtomIndexMap = RECYCLABLE_NAME_MAP_TYPE(uint32_t);

#undef RECYCLABLE_NAME_MAP_TYPE

using UsedNameSet = InlineSet<JSAtom*, 24, DefaultHasher<JSAtom*>, SystemAllocPolicy>;

// A pool of recyclable InlineTables for use in the frontend. The Parser and
// BytecodeEmitter create many maps for name analysis that are short-lived
// (i.e., for the duration of parsig or emitting a lexical scope). Making them
// recyclable cuts down significantly on allocator churn.
template <typename RepresentativeTable>
class InlineTablePool
{
    using RecyclableTables = Vector<void*, 32, SystemAllocPolicy>;

    RecyclableTables all_;
    RecyclableTables recyclable_;

    template <typename Table>
    static void assertInvariants() {
        static_assert(Table::SizeOfInlineEntries == RepresentativeTable::SizeOfInlineEntries,
                      "Only tables with the same size for inline entries are usable in the pool.");
        static_assert(mozilla::IsPod<typename Table::Table::Entry>::value,
                      "Only tables with POD values are usable in the pool.");
    }

    static RepresentativeTable* asRepresentative(void* p) {
        return reinterpret_cast<RepresentativeTable*>(p);
    }

    RepresentativeTable* allocate() {
        size_t newAllLength = all_.length() + 1;
        if (!all_.reserve(newAllLength) || !recyclable_.reserve(newAllLength))
            return nullptr;

        RepresentativeTable* table = js_new<RepresentativeTable>();
        if (table)
            all_.infallibleAppend(table);
        return table;
    }

  public:
    ~InlineTablePool() {
        purgeAll();
    }

    bool empty() const {
        return all_.empty();
    }

    void purgeAll() {
        void** end = all_.end();
        for (void** it = all_.begin(); it != end; ++it)
            js_delete(asRepresentative(*it));

        all_.clearAndFree();
        recyclable_.clearAndFree();
    }

    // Fallibly aquire one of the supported table types from the pool.
    template <typename Table>
    Table* acquire(ExclusiveContext* cx) {
        assertInvariants<Table>();

        RepresentativeTable* table;
        if (recyclable_.empty()) {
            table = allocate();
            if (!table)
                ReportOutOfMemory(cx);
        } else {
            table = asRepresentative(recyclable_.popCopy());
            table->clear();
        }
        return reinterpret_cast<Table*>(table);
    }

    // Release a table back to the pool.
    template <typename Table>
    void release(Table** table) {
        assertInvariants<Table>();

#ifdef DEBUG
        bool ok = false;
        // Make sure the table is in |all_| but not already in |recyclable_|.
        for (void** it = all_.begin(); it != all_.end(); ++it) {
            if (*it == *table) {
                ok = true;
                break;
            }
        }
        MOZ_ASSERT(ok);
        for (void** it = recyclable_.begin(); it != recyclable_.end(); ++it)
            MOZ_ASSERT(*it != *table);
#endif

        MOZ_ASSERT(recyclable_.length() < all_.length());
        // Reserved in allocateFresh.
        recyclable_.infallibleAppend(*table);
        *table = nullptr;
    }
};

class NameTablePools
{
    InlineTablePool<AtomIndexMap> mapPool_;
    InlineTablePool<UsedNameSet> setPool_;
    uint32_t activeCompilations_;

  public:
    NameTablePools()
      : activeCompilations_(0)
    { }

    bool hasActiveCompilation() const {
        return activeCompilations_ != 0;
    }

    void addActiveCompilation() {
        activeCompilations_++;
    }

    void removeActiveCompilation() {
        MOZ_ASSERT(hasActiveCompilation());
        activeCompilations_--;
    }

    template <typename Map>
    Map* acquireMap(ExclusiveContext* cx) {
        MOZ_ASSERT(hasActiveCompilation());
        return mapPool_.acquire<Map>(cx);
    }

    template <typename Set>
    Set* acquireSet(ExclusiveContext* cx) {
        MOZ_ASSERT(hasActiveCompilation());
        return setPool_.acquire<Set>(cx);
    }

    template <typename Map>
    void releaseMap(Map** map) {
        MOZ_ASSERT(hasActiveCompilation());
        mapPool_.release(map);
    }

    template <typename Set>
    void releaseSet(Set** set) {
        MOZ_ASSERT(hasActiveCompilation());
        setPool_.release(set);
    }

    void purge() {
        if (!hasActiveCompilation()) {
            mapPool_.purgeAll();
            setPool_.purgeAll();
        }
    }
};

} // namespace frontend
} // namespace js

namespace mozilla {

template <>
struct IsPod<js::frontend::DeclaredNameInfo> : TrueType {};

template <>
struct IsPod<js::frontend::NameLocation> : TrueType {};

template <>
struct IsPod<js::MaybeCheckTDZ> : TrueType {};

template <typename T>
struct IsPod<js::frontend::RecyclableAtomMapValueWrapper<T>> : IsPod<T> {};

} // namespace mozilla

#endif // frontend_NameTables_h

/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef frontend_NameMaps_h
#define frontend_NameMaps_h

#include "ds/InlineTable.h"
#include "frontend/NameAnalysisTypes.h"
#include "js/Vector.h"
#include "vm/Stack.h"

namespace js {
namespace frontend {

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
using UsedNameSet = RECYCLABLE_NAME_MAP_TYPE(UsedNameInfo);

#undef RECYCLABLE_NAME_MAP_TYPE

// A pool of recyclable InlineTables for use in the frontend. The Parser and
// BytecodeEmitter create many maps for name analysis that are short-lived
// (i.e., for the duration of parsing or emitting a lexical scope). Making
// them recyclable cuts down significantly on allocator churn.
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
        MOZ_ASSERT(*table);

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

class NameMapPool
{
    InlineTablePool<AtomIndexMap> pool_;
    uint32_t activeCompilations_;

  public:
    NameMapPool()
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
    Map* acquire(ExclusiveContext* cx) {
        MOZ_ASSERT(hasActiveCompilation());
        return pool_.acquire<Map>(cx);
    }

    template <typename Map>
    void release(Map** map) {
        MOZ_ASSERT(hasActiveCompilation());
        MOZ_ASSERT(map);
        if (*map)
            pool_.release(map);
    }

    void purge() {
        if (!hasActiveCompilation())
            pool_.purgeAll();
    }
};

} // namespace frontend
} // namespace js

namespace mozilla {

template <>
struct IsPod<js::MaybeCheckTDZ> : TrueType {};

template <typename T>
struct IsPod<js::frontend::RecyclableAtomMapValueWrapper<T>> : IsPod<T> {};

} // namespace mozilla

#endif // frontend_NameMaps_h

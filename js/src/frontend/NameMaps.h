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

using UsedNameVector = Vector<UsedName, 8, SystemAllocPolicy>;

// A pool of recyclable containers for use in the frontend. The Parser and
// BytecodeEmitter create many maps for name analysis that are short-lived
// (i.e., for the duration of parsig or emitting a lexical scope). Making them
// recyclable cuts down significantly on allocator churn.
template <typename RepresentativeCollection, typename ConcreteCollectionPool>
class CollectionPool
{
    using RecyclableCollections = Vector<void*, 32, SystemAllocPolicy>;

    RecyclableCollections all_;
    RecyclableCollections recyclable_;

    static RepresentativeCollection* asRepresentative(void* p) {
        return reinterpret_cast<RepresentativeCollection*>(p);
    }

    RepresentativeCollection* allocate() {
        size_t newAllLength = all_.length() + 1;
        if (!all_.reserve(newAllLength) || !recyclable_.reserve(newAllLength))
            return nullptr;

        RepresentativeCollection* container = js_new<RepresentativeCollection>();
        if (container)
            all_.infallibleAppend(container);
        return container;
    }

  public:
    ~CollectionPool() {
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

    // Fallibly aquire one of the supported container types from the pool.
    template <typename Collection>
    Collection* acquire(ExclusiveContext* cx) {
        ConcreteCollectionPool::template assertInvariants<Collection>();

        RepresentativeCollection* container;
        if (recyclable_.empty()) {
            container = allocate();
            if (!container)
                ReportOutOfMemory(cx);
        } else {
            container = asRepresentative(recyclable_.popCopy());
            container->clear();
        }
        return reinterpret_cast<Collection*>(container);
    }

    // Release a container back to the pool.
    template <typename Collection>
    void release(Collection** container) {
        ConcreteCollectionPool::template assertInvariants<Collection>();
        MOZ_ASSERT(*container);

#ifdef DEBUG
        bool ok = false;
        // Make sure the container is in |all_| but not already in |recyclable_|.
        for (void** it = all_.begin(); it != all_.end(); ++it) {
            if (*it == *container) {
                ok = true;
                break;
            }
        }
        MOZ_ASSERT(ok);
        for (void** it = recyclable_.begin(); it != recyclable_.end(); ++it)
            MOZ_ASSERT(*it != *container);
#endif

        MOZ_ASSERT(recyclable_.length() < all_.length());
        // Reserved in allocateFresh.
        recyclable_.infallibleAppend(*container);
        *container = nullptr;
    }
};

template <typename RepresentativeTable>
class InlineTablePool : public CollectionPool<RepresentativeTable,
                                              InlineTablePool<RepresentativeTable>>
{
  public:
    template <typename Table>
    static void assertInvariants() {
        static_assert(Table::SizeOfInlineEntries == RepresentativeTable::SizeOfInlineEntries,
                      "Only tables with the same size for inline entries are usable in the pool.");
        static_assert(mozilla::IsPod<typename Table::Table::Entry>::value,
                      "Only tables with POD values are usable in the pool.");
    }
};

class UsedNameVectorPool : public CollectionPool<UsedNameVector, UsedNameVectorPool>
{
  public:
    template <typename Vector>
    static void assertInvariants() {
        static_assert(mozilla::IsSame<Vector, UsedNameVector>::value,
                      "Only UsedNameVectors are usable in the pool.");
    }
};

class NameMapPool
{
    InlineTablePool<AtomIndexMap> mapPool_;
    UsedNameVectorPool vectorPool_;
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
    inline Map* acquire(ExclusiveContext* cx) {
        MOZ_ASSERT(hasActiveCompilation());
        return mapPool_.acquire<Map>(cx);
    }

    template <typename Map>
    inline void release(Map** map) {
        MOZ_ASSERT(hasActiveCompilation());
        MOZ_ASSERT(map);
        if (*map)
            mapPool_.release(map);
    }

    void purge() {
        if (!hasActiveCompilation()) {
            mapPool_.purgeAll();
            vectorPool_.purgeAll();
        }
    }
};

template <>
inline UsedNameVector*
NameMapPool::acquire<UsedNameVector>(ExclusiveContext* cx)
{
    MOZ_ASSERT(hasActiveCompilation());
    return vectorPool_.acquire<UsedNameVector>(cx);
}

template <>
inline void
NameMapPool::release<UsedNameVector>(UsedNameVector** vec)
{
    MOZ_ASSERT(hasActiveCompilation());
    MOZ_ASSERT(vec);
    if (*vec)
        vectorPool_.release(vec);
}

} // namespace frontend
} // namespace js

namespace mozilla {

template <>
struct IsPod<js::MaybeCheckTDZ> : TrueType {};

template <typename T>
struct IsPod<js::frontend::RecyclableAtomMapValueWrapper<T>> : IsPod<T> {};

} // namespace mozilla

#endif // frontend_NameMaps_h

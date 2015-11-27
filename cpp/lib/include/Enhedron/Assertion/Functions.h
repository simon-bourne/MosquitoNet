//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "Enhedron/Assertion/Configurable.h"

#include <algorithm>
#include <utility>
#include <type_traits>

namespace Enhedron { namespace Assertion { namespace Impl { namespace Impl_Functions {
    using std::forward;
    using std::count;
    using std::count_if;
    using std::equal;
    using std::all_of;
    using std::any_of;
    using std::none_of;
    using std::search;
    using std::remove_reference_t;

    template<typename Container, typename Value>
    auto countEqual(Container&& container, Value&& value) {
        return makeFunction(
                "countEqual",
                [] (auto&& container, auto&& value) {
                    return count(container.begin(), container.end(), forward<decltype(value)>(value));
                }
        )(forward<Container>(container), forward<Value>(value));
    }

    template<typename Container, typename Predicate>
    auto countMatching(Container&& container, Predicate&& predicate) {
        return makeFunction(
                "countMatching",
                [] (auto&& container, auto&& predicate) {
                    return count_if(container.begin(), container.end(), forward<decltype(predicate)>(predicate));
                }
        )(forward<Container>(container), forward<Predicate>(predicate));
    }

    template<typename Container, typename Predicate>
    auto allOf(Container&& container, Predicate&& predicate) {
        return makeFunction(
                "allOf",
                [] (auto&& container, auto&& predicate) {
                    return all_of(container.begin(), container.end(), forward<decltype(predicate)>(predicate));
                }
        )(forward<Container>(container), forward<Predicate>(predicate));
    }

    template<typename Container, typename Predicate>
    auto anyOf(Container&& container, Predicate&& predicate) {
        return makeFunction(
                "anyOf",
                [] (auto&& container, auto&& predicate) {
                    return any_of(container.begin(), container.end(), forward<decltype(predicate)>(predicate));
                }
        )(forward<Container>(container), forward<Predicate>(predicate));
    }

    template<typename Container, typename Predicate>
    auto noneOf(Container&& container, Predicate&& predicate) {
        return makeFunction(
                "noneOf",
                [] (auto&& container, auto&& predicate) {
                    return none_of(container.begin(), container.end(), forward<decltype(predicate)>(predicate));
                }
        )(forward<Container>(container), forward<Predicate>(predicate));
    }

    template<typename Container>
    auto length(Container&& container) {
        return makeFunction(
                "length",
                [] (auto&& container) {
                    return container.size();
                }
        )(forward<Container>(container));
    }

    template<typename MainContainer, typename StartsWithContainer>
    auto startsWith(MainContainer&& container, StartsWithContainer&& starting) {
        return makeFunction(
                "startsWith",
                [] (auto&& container, auto&& starting) {
                    return container.size() >= starting.size() &&
                           equal(starting.begin(), starting.end(), container.begin());
                }
        )(forward<MainContainer>(container), forward<StartsWithContainer>(starting));
    }

    template<typename MainContainer, typename EndsWithContainer>
    auto endsWith(MainContainer&& container, EndsWithContainer&& ending) {
        return makeFunction(
                "endsWith",
                [] (auto&& container, auto&& ending) {
                    using ContainerType = remove_reference_t<decltype(container)>;
                    auto size = static_cast<typename ContainerType::difference_type>(ending.size());
                    return container.size() >= ending.size() &&
                            equal(ending.begin(), ending.end(), container.end() - size);
                }
        )(forward<MainContainer>(container), forward<EndsWithContainer>(ending));
    }

    template<typename MainContainer, typename Contained>
    auto contains(MainContainer&& mainContainer, Contained&& contained) {
        return makeFunction(
                "contains",
                [] (auto&& mainContainer, auto&& contained) {
                    return mainContainer.end() !=
                           search(mainContainer.begin(), mainContainer.end(), contained.begin(), contained.end());
                }
        )(forward<MainContainer>(mainContainer), forward<Contained>(contained));
    }
}}}}

namespace Enhedron { namespace Assertion {
    using Impl::Impl_Functions::countEqual;
    using Impl::Impl_Functions::countMatching;
    using Impl::Impl_Functions::allOf;
    using Impl::Impl_Functions::anyOf;
    using Impl::Impl_Functions::noneOf;
    using Impl::Impl_Functions::length;
    using Impl::Impl_Functions::startsWith;
    using Impl::Impl_Functions::endsWith;
    using Impl::Impl_Functions::contains;
}}

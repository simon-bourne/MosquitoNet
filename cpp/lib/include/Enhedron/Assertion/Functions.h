#pragma once

#include "Enhedron/Assertion/Configurable.h"

#include <algorithm>
#include <utility>

namespace Enhedron { namespace Assertion { namespace Impl { namespace Impl_Functions {
    using std::forward;
    using std::count;
    using std::count_if;

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
}}}}

namespace Enhedron { namespace Assertion {
    using Impl::Impl_Functions::countEqual;
    using Impl::Impl_Functions::countMatching;
}}

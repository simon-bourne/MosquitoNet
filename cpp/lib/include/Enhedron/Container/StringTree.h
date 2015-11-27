//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "Enhedron/Util.h"

#include <string>
#include <map>
#include <utility>

#include "Enhedron/Util/Optional.h"

namespace Enhedron { namespace Container { namespace Impl { namespace StringTree {
    using std::map;
    using std::string;
    using std::move;
    using std::unique_ptr;
    using std::make_unique;

    using Util::optional;
    using Util::none;

    class StringTree final: public NoCopy {
    public:
        Out<StringTree> set(string key) {
            return set(move(key), StringTree{});
        }

        Out<StringTree> set(string key, StringTree stringTree) {
            return out(*children.emplace(move(key), make_unique<StringTree>(move(stringTree))).first->second);
        }

        optional<Out<StringTree>> get(const string& key) {
            auto childIter = children.find(key);

            if (childIter == children.end()) {
                return none;
            }

            return out(*childIter->second);
        }

        optional<const StringTree&> get(const string& key) const {
            auto childIter = children.find(key);

            if (childIter == children.end()) {
                return none;
            }

            return *childIter->second;
        }

        void clear() {
            children.clear();
        }

        size_t childCount() const {
            return children.size();
        }

        bool empty() const {
            return children.empty();
        }
    private:
        using KeyToChildren = map<string, unique_ptr<StringTree>>;
        KeyToChildren children;
    };
}}}}

namespace Enhedron { namespace Container {
    using Impl::StringTree::StringTree;
}}

// Copyright (C) 2014 Simon Bourne


#pragma once

#include "Enhedron/Util.h"

#include <string>
#include <map>
#include <utility>

#include <boost/optional/optional.hpp>

namespace Enhedron { namespace Container { namespace Impl { namespace StringTree {
    using std::map;
    using std::string;
    using std::move;
    using std::unique_ptr;
    using std::make_unique;

    using boost::optional;
    using boost::none;

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

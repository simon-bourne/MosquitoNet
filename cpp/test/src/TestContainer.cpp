//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#include "Enhedron/Test.h"
#include "Enhedron/Container/StringTree.h"

#include <string>
#include <vector>

namespace Enhedron {
    using Test::context;
    using Test::given;

    using Container::StringTree;

    static Test::Suite s("Container", context("StringTree",
        given("Set a single element on an empty StringTree", [] (auto& check) {
            StringTree tree;
            tree.set("testKey");

            check("the key is in the tree", VAL(bool(tree.get("testKey"))));
            check("an arbitrary different key is not in the tree", ! VAL(bool(tree.get("testKey1"))));
            check("the tree is not empty", ! VAL(tree.empty()));
            check("the tree has 1 child", VAL(tree.childCount()) == 1u);
        }),
        given("Set a whole path on an empty StringTree", [] (auto& check) {
            StringTree tree;
            const std::vector<std::string> path{"root", "child1", "child2"};

            auto subTreeRef = out(tree);

            for (const auto& node : path) {
                subTreeRef = subTreeRef->set(node);
            }

            subTreeRef = out(tree);

            for (const auto& node : path) {
                auto mayBeSubTreeRef = subTreeRef->get(node);

                if ( ! check("subtree exists", VAL(bool(mayBeSubTreeRef)))) {
                    break;
                }

                subTreeRef = *mayBeSubTreeRef;
            }

            check("no more subtrees exist", VAL(subTreeRef->childCount()) == 0u);
            check("the tree is not empty", ! VAL(tree.empty()));
            check("there is exactly one child", VAL(tree.childCount()) == 1u);
        })
    ));
}

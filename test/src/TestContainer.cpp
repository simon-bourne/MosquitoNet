// Copyright (C) 2014 Enhedron Ltd

#include "Enhedron/Test.h"
#include "Enhedron/Container/StringTree.h"

#include <string>
#include <vector>

namespace Enhedron {
    using Test::context;
    using Test::Given;
    using Test::When;
    using Test::Then;
    using Test::gwt;

    using Container::StringTree;

    class SetSingleOnEmpty final: public Test::GWT {
        StringTree tree;
    public:
        SetSingleOnEmpty() : Test::GWT(
                Given("an empty string tree"),
                When("we set a key"),
                Then("only that key is set")
        )
        {}

        void when() { tree.set("testKey"); }

        void then() {
            check("the key is in the tree", M_VAR(bool(tree.get("testKey"))));
            check("an arbitrary different key is not in the tree", ! M_VAR(bool(tree.get("testKey1"))));
            check("the tree is not empty", ! M_VAR(tree.empty()));
            check("the tree has 1 child", M_VAR(tree.childCount()) == 1u);
        }
    };

    class SetPathOnEmpty final: public Test::GWT {
        StringTree tree;
        const std::vector<std::string> path{"root", "child1", "child2"};
    public:
        SetPathOnEmpty() : Test::GWT(
                Given("an empty string tree"),
                When("we set a path"),
                Then("only that path is set")
        )
        {}

        void when() {
            auto subTreeRef = out(tree);

            for (const auto& node : path) {
                subTreeRef = subTreeRef->set(node);
            }
        }

        void then() {
            auto subTreeRef = out(tree);

            for (const auto& node : path) {
                auto mayBeSubTreeRef = subTreeRef->get(node);

                if ( ! check("subtree exists", M_VAR(bool(mayBeSubTreeRef)))) {
                    break;
                }

                subTreeRef = *mayBeSubTreeRef;
            }

            check("no more subtrees exist", M_VAR(subTreeRef->childCount()) == 0u);
            check("the tree is not empty", ! M_VAR(tree.empty()));
            check("there is exactly one child", M_VAR(tree.childCount()) == 1u);
        }
    };

    static Test::Unit u(
        context("Container",
            context("StringTree",
                    gwt<SetSingleOnEmpty>("SetSingleOnEmpty"),
                    gwt<SetPathOnEmpty>("SetPathOnEmpty")
            )
        )
    );
}

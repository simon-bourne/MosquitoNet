#pragma once

#include "Enhedron/Test.h"
#include "Enhedron/CommandLine/Parameters.h"
#include "Enhedron/Container/StringTree.h"

#include <string>
#include <vector>
#include <utility>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Harness {
    using std::string;
    using std::move;
    using std::vector;
    using std::runtime_error;
    using std::find;

    using CommandLine::ExitStatus;
    using CommandLine::Flag;
    using CommandLine::Arguments;

    using Container::StringTree;

    ExitStatus runTests(bool listOnly, vector<string> pathList) {
        StringTree pathTree;

        for (auto& path : pathList) {
            vector<string> pathComponentList;
            auto posDot = path.begin();

            while (true) {
                auto end = find(posDot, path.end(), '.');

                pathComponentList.emplace_back(posDot, end);

                if (end == path.end()) {
                    break;
                }

                posDot = end;
                ++posDot;
            }

            auto childPtr = out(pathTree);

            for (const auto& pathComponent : pathComponentList) {
                auto child = childPtr->get(pathComponent);

                if (child) {
                    childPtr = *child;
                }
                else {
                    childPtr = childPtr->set(pathComponent);
                }
            }

            childPtr->clear();
        }

        if (listOnly) {
            Test::list(pathTree);
        }
        else {
            if ( ! Test::run(pathTree)) {
                return ExitStatus::SOFTWARE;
            }
        }

        return ExitStatus::OK;
    }

    int run(int argc, const char* argv[]) {
        Arguments args("MosquitoNet test harness version 0.0.0");
        args.setDescription("Test Harness");
        args.setPositionalDescription("TEST_PATH");

        return args.run(argc, argv, runTests, Flag('l', "list", "List tests instead of running them."));
    }
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Harness::run;
}}

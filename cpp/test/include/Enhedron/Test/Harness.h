#pragma once

#include "Enhedron/Test.h"
#include "Enhedron/CommandLine/Parameters.h"
#include "Enhedron/Container/StringTree.h"

#include <string>
#include <vector>
#include <utility>

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/replace.hpp>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Harness {
    using std::string;
    using std::move;
    using std::vector;
    using std::runtime_error;

    using boost::replace_all_copy;
    using boost::algorithm::is_any_of;
    using boost::algorithm::split;

    using CommandLine::ExitStatus;
    using CommandLine::Flag;
    using CommandLine::Arguments;

    using Container::StringTree;

    ExitStatus runTests(bool listOnly, vector<string> pathList) {
        StringTree pathTree;

        for (auto& path : pathList) {
            vector<string> pathComponentList;
            split(pathComponentList, path, is_any_of("."));
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
        return Arguments("Test Harness", "").run(argc, argv, runTests, Flag("list"));
    }
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Harness::run;
}}

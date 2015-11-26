#pragma once

#include "Enhedron/Test.h"
#include "Enhedron/CommandLine/Parameters.h"
#include "Enhedron/Container/StringTree.h"

#include <string>
#include <vector>
#include <utility>
#include <algorithm>
#include <locale>
#include <cctype>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Harness {
    using std::string;
    using std::move;
    using std::vector;
    using std::runtime_error;
    using std::find;
    using std::tolower;
    using std::locale;
    using std::transform;

    using CommandLine::ExitStatus;
    using CommandLine::Flag;
    using CommandLine::Option;
    using CommandLine::Name;
    using CommandLine::Arguments;

    using Container::StringTree;

    Verbosity parseVerbosity(string v) {
        transform(v.begin(), v.end(), v.begin(),
                  [](char c) { return tolower(c, locale()); } );

        if (v == "silent") return Verbosity::SILENT;
        if (v == "summary") return Verbosity::SUMMARY;
        if (v == "contexts") return Verbosity::CONTEXTS;
        if (v == "fixtures") return Verbosity::FIXTURES;
        if (v == "sections") return Verbosity::SECTIONS;
        if (v == "exhaustive_sections") return Verbosity::EXHAUSTIVE_SECTIONS;
        if (v == "checks") return Verbosity::CHECKS;
        if (v == "checks_expression") return Verbosity::CHECKS_EXPRESSION;
        if (v == "variables") return Verbosity::VARIABLES;

        throw runtime_error("Unknown verbosity \"" + v + "\"");
    }

    ExitStatus runTests(bool listOnly, string verbosityString, vector<string> pathList) {
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

        Verbosity verbosity = parseVerbosity(verbosityString);

        if (listOnly) {
            Test::list(pathTree, verbosity);
        }
        else {
            if ( ! Test::run(pathTree, verbosity)) {
                return ExitStatus::SOFTWARE;
            }
        }

        return ExitStatus::OK;
    }

    int run(int argc, const char* argv[]) {
        Arguments args("MosquitoNet test harness version 0.0.0");
        args.setDescription("Test Harness");
        args.setPositionalDescription("TEST_PATH");

        return args.run(
                argc, argv, runTests,
                Flag('l', "list", "List tests instead of running them."),
                Option<string>(Name('v', "verbosity", "Set the verbosity"),
                               "silent|summary|contexts|fixtures|sections|exhaustive-sections|checks_description|checks_expression|variables",
                               "contexts"
                )
        );
    }
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Harness::run;
}}

//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "Enhedron/Test.h"
#include "Enhedron/CommandLine/Parameters.h"

#include <string>
#include <vector>
#include <utility>
#include <algorithm>
#include <locale>
#include <cctype>
#include <regex>
#include <memory>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Harness {
    using std::string;
    using std::move;
    using std::vector;
    using std::shared_ptr;
    using std::make_shared;
    using std::runtime_error;
    using std::find_first_of;
    using std::tolower;
    using std::locale;
    using std::transform;
    using std::regex;

    using CommandLine::ExitStatus;
    using CommandLine::Flag;
    using CommandLine::Option;
    using CommandLine::Name;
    using CommandLine::Arguments;

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
        vector<shared_ptr<vector<regex>>> pathRegexs;

        for (auto& path : pathList) {
            vector<regex> pathComponentList;
            auto separatorPos = path.begin();
            auto matchChars = "/\\";
            auto matchCharsEnd = matchChars + 2;
            auto pathEnd = path.end();

            while (true) {
                auto end = find_first_of(separatorPos, pathEnd, matchChars, matchCharsEnd);

                while (end != pathEnd && end + 1 != pathEnd && *end == '\\') {
                    ++end;

                    if (*end == '/') {
                        copy(end, pathEnd, end - 1);
                    }
                    else {
                        ++end;
                    }

                    end = find_first_of(end, pathEnd, matchChars, matchCharsEnd);
                }

                if (separatorPos != end) {
                    pathComponentList.emplace_back(separatorPos, end);
                }

                if (end == pathEnd) {
                    break;
                }

                separatorPos = end;
                ++separatorPos;
            }

            pathRegexs.emplace_back(make_shared<vector<regex>>(move(pathComponentList)));
        }

        Verbosity verbosity = parseVerbosity(verbosityString);

        if (listOnly) {
            Test::list(pathRegexs, verbosity);
        }
        else {
            if ( ! Test::run(pathRegexs, verbosity)) {
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

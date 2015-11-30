//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#include "Enhedron/CommandLine/Parameters.h"
#include "Enhedron/Test.h"
#include "Enhedron/Util.h"

#include <initializer_list>
#include <sstream>
#include <vector>
#include <string>

namespace Enhedron { namespace Impl_TestCommandLine {
    using namespace CommandLine;
    using namespace Test;

    using std::ostringstream;
    using std::vector;
    using std::string;

    using Args = vector<const char*>;

    void testEmpty(
            Check& check,
            vector<const char*> args,
            ExitStatus exitStatus,
            const char* expectedOutput,
            const char* expectedErrorOutput
    ) {
        ostringstream helpOut;
        ostringstream errorOut;

        auto exitStatusCheck = Arguments(out(helpOut), out(errorOut), "").run(
            static_cast<int>(args.size()),
            args.data(),
            [&] (vector<string> positional) {
                check(VAR(positional.size()) == 0u);
                return exitStatus;
            }
        );

        check(VAR(helpOut.str()) == expectedOutput);
        check(VAR(errorOut.str()) == expectedErrorOutput);
        check(VAR(exitStatusCheck) == static_cast<int>(exitStatus));
    }

    static Test::Suite s("CommandLine",
         given("NoExeName", testEmpty, Args{}, ExitStatus::SOFTWARE, "", "unknown: argc is 0.\n"),
         given("NullArgv", testEmpty, Args{nullptr}, ExitStatus::SOFTWARE, "", "unknown: argv has null value.\n"),
         given("Empty", testEmpty, Args{""}, ExitStatus::OK, "", ""),
         given("ExitStatus", testEmpty, Args{""}, ExitStatus::CONFIG, "", ""),

         given("Help", [] (Check& check) {
             const char* argv[] = { "exeName", "--help" };
             ostringstream helpOut;
             ostringstream errorOut;

             Arguments args(out(helpOut), out(errorOut), "");
             args.setDescription("Description");
             args.setNotes("Notes");

             auto exitStatus = args.run(
                 2,
                 argv,
                 [&] (vector<string> positional) {
                     check.fail(VAR("Expected help"));
                     return ExitStatus::OK;
                 }
             );

             check(VAR(helpOut.str()) ==
                 "Usage: exeName [OPTION]...\n\n"
                 "Description\n\n"
                 "  --help        Display this help message.\n"
                 "  --version     Display version information.\n\n"
                 "Notes\n\n");
             check(VAR(errorOut.str()) == "");
             check(VAR(exitStatus) == static_cast<int>(ExitStatus::OK));
         }),

         given("StringArgs", [] (Check& check) {
             const char* argv[] = { "exeName", "--string2", "abc", "--string", "xyz" };
             ostringstream helpOut;
             ostringstream errorOut;

             auto exitStatus = Arguments(out(helpOut), out(errorOut), "").run(
                 5, argv,
                 [&](const string& arg, const string& arg1, vector<string> positional) {
                     check(VAR(arg) == "xyz");
                     check(VAR(arg1) == "abc");
                     check(VAR(positional.size()) == 0u);

                     return ExitStatus::OK;
                 },
                 Option<string>(Name("string"), "value"),
                 Option<string>(Name("string2"), "value2")
             );

             check(VAR(helpOut.str()) == "");
             check(VAR(errorOut.str()) == "");
             check(VAR(exitStatus) == static_cast<int>(ExitStatus::OK));
         }),

         given("Description", [] (Check& check) {
             const char* argv[] = { "exeName", "--help" };
             ostringstream helpOut;
             ostringstream errorOut;

             auto exitStatus = Arguments(out(helpOut), out(errorOut), "", 40).run(
                     2, argv,
                     [&](const string& arg1, const string& arg2, vector<string> positional) {
                         check.fail(VAR("Should show help"));

                         return ExitStatus::OK;
                     },
                     Option<string>(Name("string1", "String 1 description"), "string1Value"),
                     Option<string>(Name('s', "string2", "String2descriptionShouldBeHyphenated"), "string2Value")
             );

             check(VAR(helpOut.str()) ==
                           "Usage: exeName [OPTION]...\n\n"
                           "  --string1 <string1Value>\n"
                           "                    String 1 description\n\n"
                           "  -s, --string2 <string2Value>\n"
                           "                    String2descriptionS-\n"
                           "                    houldBeHyphenated\n\n"
                           "  --help            Display this help\n"
                           "                    message.\n\n"
                           "  --version         Display version\n"
                           "                    information.\n\n\n"
             );
             check(VAR(errorOut.str()) == "");
             check(VAR(exitStatus) == static_cast<int>(ExitStatus::OK));
         }),

        given("VectorArgs", [] (Check& check) {
            const char* argv[] = { "exeName", "--vector", "value0", "--vector", "value1"};
            ostringstream helpOut;
            ostringstream errorOut;

            auto exitStatus = Arguments(out(helpOut), out(errorOut), "").run(
                    5, argv,
                    [&](const vector<string>& vectorOption, vector<string> positional) {
                        if (check(VAR(vectorOption.size()) == 2u)) {
                            check(VAR(vectorOption[0]) == "value0");
                            check(VAR(vectorOption[1]) == "value1");
                        }

                        return ExitStatus::OK;
                    },
                    Option<vector<string>>(Name("vector", ""), "")
            );

            check(VAR(helpOut.str()) == "");
            check(VAR(errorOut.str()) == "");
            check(VAR(exitStatus) == static_cast<int>(ExitStatus::OK));
        })
    );
}}

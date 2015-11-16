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

    void testEmpty(Check& check, vector<const char*> args, ExitStatus exitStatus, const char* expectedOutput) {
        ostringstream output;

        auto exitStatusCheck = Arguments(out(output), "", "").run(
            static_cast<int>(args.size()),
            args.data(),
            [&] (vector<string> positional) {
                check(VAL(positional.size()) == 0u);
                return exitStatus;
            }
        );

        check(VAL(output.str()) == expectedOutput);
        check(VAL(exitStatusCheck) == static_cast<int>(exitStatus));
    }

    static Test::Suite s("CommandLine",
         simple("NoExeName", testEmpty, Args{}, ExitStatus::USAGE, "Error: argc is 0.\n"),
         simple("NullArgv", testEmpty, Args{nullptr}, ExitStatus::USAGE, "Error: argv has null value.\n"),
         simple("Empty", testEmpty, Args{""}, ExitStatus::OK, ""),
         simple("ExitStatus", testEmpty, Args{""}, ExitStatus::CONFIG, ""),

         simple("Help", [] (Check& check) {
             const char* argv[] = { "exeName", "--help" };
             ostringstream output;

             auto exitStatus = Arguments(out(output), "Description", "Notes").run(
                 2,
                 argv,
                 [&] (vector<string> positional) {
                     check.fail(VAL("Expected help"));
                     return ExitStatus::OK;
                 }
             );

             check(VAL(output.str()) ==
                 "Usage: exeName [OPTION]...\n\n"
                 "Description\n\n"
                 " Standard Options:\n\n"
                 "  --help        Display this help message.\n"
                 "  --version     Display version information.\n\n"
                 "Notes\n");
             check(VAL(exitStatus) == static_cast<int>(ExitStatus::OK));
         }),

         simple("StringArgs", [] (Check& check) {
             const char* argv[] = { "exeName", "--string2", "abc", "--string", "xyz" };
             ostringstream output;

             auto exitStatus = Arguments(out(output), "", "").run(
                 5, argv,
                 [&](const string& arg, const string& arg1, vector<string> positional) {
                     check(VAL(arg) == "xyz");
                     check(VAL(arg1) == "abc");
                     check(VAL(positional.size()) == 0u);

                     return ExitStatus::OK;
                 },
                 Option<string>("string"),
                 Option<string>("string2")
             );

             check(VAL(output.str()) == "");
             check(VAL(exitStatus) == static_cast<int>(ExitStatus::OK));
         })
    );
}}

#include "Enhedron/CommandLine/Options.h"
#include "Enhedron/Test.h"

#include <initializer_list>

namespace Enhedron { namespace Impl_TestCommandLine {
    using namespace CommandLine;
    using namespace Test;

    using std::initializer_list;

    void checkIfHelpRequested(Check& check, initializer_list<const char*> argv) {
        readCommandLine(
                "",
                static_cast<int>(argv.size()),
                argv.begin(),
                [&](const Help& help) {
                    return ExitStatus::OK;
                }
        );
    }

    static Test::Suite s("CommandLine",
        simple("Empty", checkIfHelpRequested, initializer_list<const char*>{"exeName"})
    );
}}
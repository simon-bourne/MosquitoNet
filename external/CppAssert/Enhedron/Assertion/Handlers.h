// Copyright (C) 2014 Simon Bourne

#pragma once

#include <exception>
#include <vector>
#include <string>

#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Log.h"
#include "Enhedron/Util.h"

namespace Enhedron { namespace Assertion { namespace Impl { namespace Handlers {
    using std::terminate;
    using std::vector;
    using std::string;
    using std::cerr;

    static Logger log("Assert");

    void failWithSourceLocation(
            const char *name,
            const char *message,
            const char *file,
            int line,
            const char *functionName
    );

    struct TerminateInDebug final : public NoCopy {
        static constexpr const char *name = "assert";

        static void run() {
#ifndef NDEBUG
            terminate();
#endif
        }
    };

    template<typename FailureAction>
    class LogFailureHandler {
    public:
        static void handleCheckFailure(const string &expressionText, const vector <Variable> &variableList) {
            {
                auto l(log.errorBlock(FailureAction::name, "type", "assertFailed", "expression", expressionText));
                cerr << FailureAction::name << " failed: " << expressionText << "\n";

                for (const auto &variable : variableList) {
                    log.error("variable", "name", variable.name(), "value", variable.value(), "file",
                              variable.file(), "line", variable.line());
                    cerr << "    " << variable.name() << " = " << variable.value() <<
                            ": in file " << variable.file() << ", line " << variable.line() << "\n";
                }

                cerr.flush();
            }

            FailureAction::run();
        }
    };
}}}}

namespace Enhedron { namespace Assertion {
    using Impl::Handlers::LogFailureHandler;
    using Impl::Handlers::TerminateInDebug;
    using Impl::Handlers::failWithSourceLocation;
}}

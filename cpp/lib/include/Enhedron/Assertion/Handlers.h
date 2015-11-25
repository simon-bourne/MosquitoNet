// Copyright (C) 2014 Simon Bourne

#pragma once

#include <exception>
#include <vector>
#include <string>
#include <iostream>

#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Util.h"

namespace Enhedron { namespace Assertion { namespace Impl { namespace Handlers {
    using std::terminate;
    using std::vector;
    using std::string;
    using std::cout;

    struct TerminateInDebug final : public NoCopy {
        static constexpr const char *name = "assert";

        static void run() {
#ifndef NDEBUG
            terminate();
#endif
        }
    };

    template<typename FailureAction>
    struct CoutFailureHandler final: FailureHandler {
        virtual ~CoutFailureHandler() override {}

        virtual void handleCheckFailure(const string &expressionText, const vector <Variable> &variableList) override {
            {
                cout << FailureAction::name << " failed: " << expressionText << "\n";

                for (const auto &variable : variableList) {
                    cout << "    " << variable.name() << " = " << variable.value() <<
                            ": in file " << variable.file() << ", line " << variable.line() << "\n";
                }

                cout.flush();
            }

            FailureAction::run();
        }
    };
}}}}

namespace Enhedron { namespace Assertion {
    using Impl::Handlers::CoutFailureHandler;
    using Impl::Handlers::TerminateInDebug;
}}

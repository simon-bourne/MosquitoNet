// Copyright (C) 2014 Simon Bourne

#pragma once

#include "Enhedron/Assertion/Configurable.h"

#include <vector>
#include <string>
#include <iostream>
#include <utility>

namespace Enhedron { namespace Impl { namespace Impl_Assertion {
    using std::move;
    using std::terminate;
    using std::vector;
    using std::string;
    using std::cerr;

    using namespace ::Enhedron::Assertion;

    struct CerrFailureHandler final: FailureHandler {
        virtual ~CerrFailureHandler() override {}

        virtual void fail(const string &expressionText, const vector <Variable> &variableList) override {
            cerr << "Assert failed: " << expressionText << "\n";

            for (const auto &variable : variableList) {
                cerr << "    " << variable.name() << " = " << variable.value() <<
                ": in file " << variable.file() << ", line " << variable.line() << "\n";
            }

            cerr.flush();

            #ifndef NDEBUG
                terminate();
            #endif
        }
    };

    static CerrFailureHandler failureHandler;

    template<typename Expression, typename... ContextVariableList>
    void Assert(Expression &&expression, ContextVariableList &&... contextVariableList) {
        CheckWithFailureHandler(out(failureHandler), move(expression), move(contextVariableList)...);
    }

    template<typename Exception, typename Expression, typename... ContextVariableList>
    void AssertThrows(Expression &&expression, ContextVariableList &&... contextVariableList) {
        CheckThrowsWithFailureHandler<Exception>(out(failureHandler), move(expression), move(contextVariableList)...);
    }
}}}

namespace Enhedron {
    using Impl::Impl_Assertion::Assert;
}

#define VAL M_ENHEDRON_VAL

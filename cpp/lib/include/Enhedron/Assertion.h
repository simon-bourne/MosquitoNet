//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Util/Optional.h"

#include <vector>
#include <string>
#include <iostream>
#include <utility>

namespace Enhedron { namespace Impl { namespace Impl_Assertion {
    using ::Enhedron::Util::optional;

    using std::forward;
    using std::terminate;
    using std::vector;
    using std::string;
    using std::cerr;

    using namespace ::Enhedron::Assertion;

    struct CerrFailureHandler final: FailureHandler {
        virtual ~CerrFailureHandler() override {}

        virtual bool notifyPassing() const override { return false; }

        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override { }

        virtual void fail(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
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

    template<typename... Args>
    void Assert(Args&&... args) {
        CheckWithFailureHandler(out(failureHandler), forward<Args>(args)...);
    }

    template<typename Exception, typename... Args>
    void AssertThrows(Args&&... args) {
        CheckThrowsWithFailureHandler<Exception>(out(failureHandler), forward<Args>(args)...);
    }
}}}

namespace Enhedron {
    using Impl::Impl_Assertion::Assert;
}

#define VAR M_ENHEDRON_VAL

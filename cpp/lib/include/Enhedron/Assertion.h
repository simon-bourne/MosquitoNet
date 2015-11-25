// Copyright (C) 2014 Simon Bourne

#pragma once

#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Assertion/Handlers.h"

#include <utility>

namespace Enhedron { namespace Impl { namespace Impl_Assertion {
    using std::move;

    using namespace ::Enhedron::Assertion;

    static CoutFailureHandler<TerminateInDebug> failureHandler;

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

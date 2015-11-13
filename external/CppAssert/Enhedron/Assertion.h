// Copyright (C) 2014 Simon Bourne

#pragma once

#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Assertion/Handlers.h"

#include <utility>

namespace Enhedron { namespace Impl { namespace Assertion {
    using std::move;

    using namespace ::Enhedron::Assertion;

    template<typename Expression, typename... ContextVariableList>
    void Assert(Expression &&expression, ContextVariableList &&... contextVariableList) {
        CheckWithFailureHandler<LogFailureHandler<TerminateInDebug>>(move(expression),
                                                                     move(contextVariableList)...);
    }

    template<typename Exception, typename Expression, typename... ContextVariableList>
    void AssertThrows(Expression &&expression, ContextVariableList &&... contextVariableList) {
        CheckThrowsWithFailureHandler<LogFailureHandler<TerminateInDebug>, Exception>(move(expression),
                                                                                      move(contextVariableList)...);
    }
}}}

namespace Enhedron {
    using Impl::Assertion::Assert;
}

#define VAR M_ENHEDRON_VAR

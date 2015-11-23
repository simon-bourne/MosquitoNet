// Copyright (C) 2014 Simon Bourne

#pragma once

#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Assertion/Handlers.h"

#include <utility>

namespace Enhedron { namespace Impl { namespace Assertion {
    using std::move;

    using namespace ::Enhedron::Assertion;

    // TODO: CheckWithFailureHandler should take a virtual failure handler. Assert can use a global one
    // if it wishes. Check can use an object based one.
    template<typename Expression, typename... ContextVariableList>
    void Assert(Expression &&expression, ContextVariableList &&... contextVariableList) {
        CheckWithFailureHandler<CoutFailureHandler<TerminateInDebug>>(move(expression),
                                                                     move(contextVariableList)...);
    }

    template<typename Exception, typename Expression, typename... ContextVariableList>
    void AssertThrows(Expression &&expression, ContextVariableList &&... contextVariableList) {
        CheckThrowsWithFailureHandler<CoutFailureHandler<TerminateInDebug>, Exception>(move(expression),
                                                                                      move(contextVariableList)...);
    }
}}}

namespace Enhedron {
    using Impl::Assertion::Assert;
}

#define VAL M_ENHEDRON_VAL

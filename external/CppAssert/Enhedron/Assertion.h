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

#define M_VAR(expression) \
        M_ENHEDRON_VAR(expression)

#define M_VOID(expression) \
        M_ENHEDRON_VOID(expression)

#define M_FAIL(expr) \
  (::Enhedron::Assertion::failWithSourceLocation("fail", (#expr), __FILE__, __LINE__, __func__))

#ifdef NDEBUG
    #define M_DEBUG(expression) \
        { static_cast<void>(0); }
#else
    #define M_DEBUG(expression) \
        { expression; }
#endif // NDEBUG


#ifdef M_ENHEDRON_DEBUG_EXTRA
    #define M_DEBUG_EXTRA(expression) \
        { expression; }
#else
    #define M_DEBUG_EXTRA(expression) \
        { static_cast<void>(0); }
#endif

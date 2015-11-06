// Copyright (C) 2014 Simon Bourne

#pragma once

#include <stddef.h>
#include <vector>
#include <string>

#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Log.h"
#include "Enhedron/Util.h"

namespace Enhedron { namespace Impl { namespace Assertion {
    using std::terminate;
    using std::vector;
    using std::string;

    using namespace Enhedron::Assertion;

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

                for (const auto &variable : variableList) {
                    log.error("variable", "name", variable.name(), "value", variable.value(), "file",
                              variable.file(), "line", variable.line());
                }
            }

            FailureAction::run();
        }
    };

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
    using Impl::Assertion::LogFailureHandler;
    using Impl::Assertion::failWithSourceLocation;
}

#define M_VAR(expression) \
        BOURNEODB_VAR(expression)

#define M_VOID(expression) \
        BOURNEODB_VOID(expression)

#define M_FAIL(expr) \
  (Enhedron::Impl::Assert::failWithSourceLocation("fail", (#expr), __FILE__, __LINE__, __func__))

#ifdef NDEBUG
    #define M_DEBUG(expression) \
        { static_cast<void>(0); }
#else
    #define M_DEBUG(expression) \
        { expression; }
#endif // NDEBUG


#ifdef BOURNEODB_DEBUG_EXTRA
    #define M_DEBUG_EXTRA(expression) \
        { expression; }
#else
    #define M_DEBUG_EXTRA(expression) \
        { static_cast<void>(0); }
#endif

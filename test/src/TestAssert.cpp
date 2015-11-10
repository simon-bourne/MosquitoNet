// Copyright (C) 2014 Enhedron Ltd

#include "Enhedron/Test.h"
#include "Enhedron/Assertion.h"

#include <utility>
#include <string>
#include <vector>
#include <stdexcept>
#include <functional>

#include <boost/optional.hpp>

namespace Enhedron {
    using Test::context;
    using Test::simple;
    using Test::Check;

    using std::string;
    using std::vector;
    using std::move;
    using std::exception;
    using std::runtime_error;
    using std::logical_and;
    using std::logic_error;

    using boost::optional;

    using namespace Assertion;

    class FailureHandler {
    public:
        struct Failure {
            string expressionText;
            vector<Variable> variableList;
        };
    private:
        static optional<Failure> failure_;
    public:
        static void handleCheckFailure(string expressionText, vector<Variable> variableList) {
            if (failure_) {
                throw runtime_error("Multiple failures");
            }

            failure_ = Failure{move(expressionText), move(variableList)};
        }

        static void reset() {
            failure_.reset();
        }

        static const optional<Failure>& failure() { return failure_; }
    };

    optional<FailureHandler::Failure> FailureHandler::failure_;

    template<typename Expression, typename... ContextVariableList>
    void testAssert(Expression &&expression, ContextVariableList &&... contextVariableList) {
        CheckWithFailureHandler<FailureHandler>(move(expression), move(contextVariableList)...);
    }

    template<typename Exception, typename Expression, typename... ContextVariableList>
    void testAssertThrows(Expression &&expression, ContextVariableList &&... contextVariableList) {
        CheckThrowsWithFailureHandler<FailureHandler, Exception>(move(expression),
                move(contextVariableList)...);
    }

    template<typename Expression>
    void expectSuccess(Check& check, Expression expression) {
        FailureHandler::reset();

        try {
            testAssert(move(expression));

            check( ! M_VAR(bool(FailureHandler::failure())));
        }
        catch (const exception& e){
            check.fail(M_VAR(e.what()));
        }
    }

    template<typename Expression>
    void expectFailure(Check& check, Expression expression, const char* expressionText = nullptr) {
        FailureHandler::reset();

        try {
            testAssert(move(expression));
            auto& failure = FailureHandler::failure();

            if (check(M_VAR(bool(failure))) && expressionText) {
                check(M_VAR(failure->expressionText) == expressionText);
            }
        }
        catch (const exception& e){
            check.fail(M_VAR(e.what()));
        }
    }

    template<typename Operator, typename Value>
    void testBooleanOperator(Check& check, Value lhs, Value rhs) {
        Operator op;
        bool result = op(lhs, rhs);

        if (result) {
            expectSuccess(check, op(M_VAR(lhs), M_VAR(rhs)));
            expectSuccess(check, op(lhs, M_VAR(rhs)));
            expectSuccess(check, op(M_VAR(lhs), rhs));
        }
        else {
            expectFailure(check, op(M_VAR(lhs), M_VAR(rhs)));
            expectFailure(check, op(lhs, M_VAR(rhs)));
            expectFailure(check, op(M_VAR(lhs), rhs));
        }
    }

    template<typename Operator>
    void testBooleanOperator(Check& check) {
        testBooleanOperator<Operator>(check, false, false);
        testBooleanOperator<Operator>(check, false, true);
        testBooleanOperator<Operator>(check, true, false);
        testBooleanOperator<Operator>(check, true, true);
    }

    template<typename Operator, typename Value>
    void testOperator(Check& check, Value lhs, Value rhs) {
        Operator op;
        auto result = op(lhs, rhs);

        expectSuccess(check, op(M_VAR(lhs), M_VAR(rhs)) == result);
        expectSuccess(check, op(lhs, M_VAR(rhs)) == result);
        expectSuccess(check, op(M_VAR(lhs), rhs) == result);
    }

    template<typename Operator>
    void testNumericOperator(Check& check) {
        for (int i = -10; i < 10; ++i) {
            for (int j = -10; j < 10; ++j) {
                testOperator<Operator>(check, i, j);
            }
        }
    }

    template<typename Operator>
    void testNonZeroDenominator(Check& check) {
        for (int i = -10; i < 10; ++i) {
            for (int j = -10; j < -1; ++j) {
                testOperator<Operator>(check, i, j);
            }

            for (int j = 1; j < 10; ++j) {
                testOperator<Operator>(check, i, j);
            }
        }
    }

    template<typename Exception, typename Expression>
    void expectException(Check& check, Expression expression, const char* expressionText) {
        FailureHandler::reset();

        try {
            testAssertThrows<Exception>(move(expression));

            auto& failure = FailureHandler::failure();

            if (check(M_VAR(bool(failure))) && expressionText) {
                check(M_VAR(failure->expressionText) == expressionText);
            }
        }
        catch (const exception& e) {
            check.fail(M_VAR(e.what()));
        }
    }

    static Test::Unit u(
        context("Assert",
            simple("Success", [] (Check& check) {
                expectSuccess(check, M_VAR(true));
                expectSuccess(check, !M_VAR(false));
                expectSuccess(check, !M_VAR(false) && !M_VAR(false));
            }),
            simple("Failure", [] (Check& check) {
                expectFailure(check, M_VAR(false), "false");
                expectFailure(check, M_VAR(false) || M_VAR(false), "(false || false)");
            }),
            simple("ThrowSucceeds", [] (Check& check) {
                testAssertThrows<exception>(M_VOID([] { throw runtime_error("test"); }));
                testAssertThrows<runtime_error>(M_VOID([] { throw runtime_error("test"); }));
            }),
            simple("ThrowFails", [] (Check& check) {
                expectException<logic_error>(
                        check,
                        M_VOID([] { throw runtime_error("test"); }),
                        "[] { throw runtime_error(\"test\"); }"
                    );

                expectException<runtime_error>(
                        check,
                        M_VOID([] {} ),
                        "[] {}"
                );

            }),
            simple("BinaryBoolean", [] (Check& check) {
                testBooleanOperator<std::logical_and<void>>(check);
                testBooleanOperator<std::logical_or<void>>(check);
            }),
            simple("Comparison", [] (Check& check) {
                testNumericOperator<std::equal_to<void>>(check);
                testNumericOperator<std::not_equal_to<void>>(check);
                testNumericOperator<std::less<void>>(check);
                testNumericOperator<std::less_equal<void>>(check);
                testNumericOperator<std::greater<void>>(check);
                testNumericOperator<std::greater_equal<void>>(check);
            }),
            simple("Arithmetic", [] (Check& check) {
                testNumericOperator<std::plus<void>>(check);
                testNumericOperator<std::minus<void>>(check);
                testNumericOperator<std::multiplies<void>>(check);

                testNonZeroDenominator<std::modulus<void>>(check);
                testNonZeroDenominator<std::divides<void>>(check);
            }),
            simple("Bitwise", [] (Check& check) {
                testNumericOperator<std::bit_and<void>>(check);
                testNumericOperator<std::bit_or<void>>(check);
                testNumericOperator<std::bit_xor<void>>(check);
            })
        )
    );
}

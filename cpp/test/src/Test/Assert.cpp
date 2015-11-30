//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#include "Enhedron/Test.h"
#include "Enhedron/Assertion.h"
#include "Enhedron/Util/Optional.h"

#include <utility>
#include <string>
#include <vector>
#include <stdexcept>
#include <functional>
#include <algorithm>

namespace Enhedron {
    using Test::context;
    using Test::given;
    using Test::Check;

    using std::string;
    using std::vector;
    using std::move;
    using std::forward;
    using std::exception;
    using std::runtime_error;
    using std::logical_and;
    using std::logic_error;
    using std::is_same;
    using std::reference_wrapper;
    using std::count;

    using Util::optional;

    using namespace Assertion;

    class RecordFailures final: public FailureHandler {
    public:
        struct Failure {
            string expressionText;
            vector<Variable> variableList;
        };
    private:
        optional<Failure> failure_;
    public:
        virtual ~RecordFailures() override {}

        virtual bool notifyPassing() const override { return false; }

        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override { }

        virtual void fail(optional<string> description, const string& expressionText, const vector<Variable>& variableList) override {
            if (failure_) {
                throw runtime_error("Multiple failures");
            }

            failure_ = Failure{expressionText, variableList};
        }

        const optional<Failure>& failure() { return failure_; }
    };

    template<typename Exception, typename Expression, typename... ContextVariableList>
    void testAssertThrows(
            Out<FailureHandler> failureHandler,
            Expression &&expression,
            ContextVariableList &&... contextVariableList
    ) {
        CheckThrowsWithFailureHandler<Exception>(failureHandler, move(expression), move(contextVariableList)...);
    }

    class MoveTracker final: public NoCopy {
    public:
        MoveTracker() = default;
        MoveTracker(const MoveTracker&) = default;
        MoveTracker& operator=(const MoveTracker&) = default;

        MoveTracker(MoveTracker&& source) : moved_(source.moved_) {
            source.moved_ = true;
        }

        MoveTracker& operator=(MoveTracker&& source) {
            moved_ = source.moved_;
            source.moved_ = true;

            return *this;
        }

        bool moved() const { return moved_; }
    private:
        bool moved_ = false;
    };

    template<typename Expression>
    void expectFailure(Check& check, Expression expression, const char* expressionText = nullptr) {
        RecordFailures recordFailures;

        try {
            CheckWithFailureHandler(out(recordFailures), move(expression));
            auto& failure = recordFailures.failure();

            if (check(VAR(bool(failure))) && expressionText) {
                check(VAR(failure->expressionText) == expressionText);
            }
        }
        catch (const exception& e){
            check.fail(VAR(e.what()));
        }
    }

    template<typename Operator, typename Value>
    void testBooleanOperator(Check& check, Value lhs, Value rhs) {
        Operator op;
        bool result = op(lhs, rhs);

        if (result) {
            check(op(VAR(lhs), VAR(rhs)));
            check(op(lhs, VAR(rhs)));
            check(op(VAR(lhs), rhs));
        }
        else {
            expectFailure(check, op(VAR(lhs), VAR(rhs)));
            expectFailure(check, op(lhs, VAR(rhs)));
            expectFailure(check, op(VAR(lhs), rhs));
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

        check(op(VAR(lhs), VAR(rhs)) == result);
        check(op(lhs, VAR(rhs)) == result);
        check(op(VAR(lhs), rhs) == result);
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
        RecordFailures recordFailures;

        try {
            testAssertThrows<Exception>(out(recordFailures), move(expression));
            auto& failure = recordFailures.failure();

            if (check(VAR(bool(failure))) && expressionText) {
                check(VAR(failure->expressionText) == expressionText);
            }
        }
        catch (const exception& e) {
            check.fail(VAR(e.what()));
        }
    }

    int sum3(int x, int y, int z) {
        return x + y + z;
    }

    int overloaded(int x) {
        return x;
    }

    double overloaded(double x) {
        return x + 1.0;
    }

    template<typename... Args>
    auto overloadedProxy(Args&&... args) {
        return makeFunction(
                "overloaded",
                [] (auto&&... args) { return overloaded(forward<decltype(args)>(args)...); }
        )(forward<Args>(args)...);
    }

    int id(int x) { return x; }

    bool equals0(int x) { return x == 0; }
    bool equals1(int x) { return x == 1; }

    static Test::Suite s("Assert",
        given("Success", [] (Check& check) {
            check(VAR(true));
            check(! VAR(false));
            check(! VAR(false) && !VAR(false));
            check(VAR(sum3)(1, 2, 3) == 6);
        }),
        given("Failure", [] (Check& check) {
            expectFailure(check, VAR(false), "false");
            expectFailure(check, VAR(false) || VAR(false), "(false || false)");

            int a = 1;
            expectFailure(check, VAR(sum3)(VAR(a), 2, 3) == 7, "(sum3(a, 2, 3) == 7)");
        }),
        given("Overloaded", [] (Check& check) {
            check(overloadedProxy(1) == overloaded(1));
            check(overloadedProxy(1.0) == overloaded(1.0));

            int a = 1;
            expectFailure(check, overloadedProxy(VAR(a)) == 2, "(overloaded(a) == 2)");
        }),
        given("countMatching, check it works with lambda predicates", [] (Check& check) {
            vector<int> intVec{ 1, 2, 3 };
            auto zeroMatcher = [] (const int value) { return value == 0; };
            check(countMatching(intVec, [] (const int& value) { return value == 1; }) == VAR(1));
            check(countMatching(intVec, zeroMatcher) == VAR(0));
            expectFailure(
                    check,
                    countMatching(intVec, VAR(zeroMatcher)) == VAR(1),
                    "(countMatching([1, 2, 3], zeroMatcher) == 1)"
            );
        }),
        given("containerUtils", [] (Check& check) {
            const vector<int> intVec{ 1, 2, 3 };
            const vector<int> constVec{ 1, 1, 1, 1 };

            check.when("countEqual", [&] {
                check(countEqual(intVec, 1) == VAR(1));
                check(countEqual(intVec, 0) == VAR(0));
                expectFailure(check, countEqual(intVec, 0) == VAR(1), "(countEqual([1, 2, 3], 0) == 1)");
            });

            check.when("countMatching", [&] {
                check(countMatching(intVec, equals1) == VAR(1));
                check(countMatching(intVec, equals0) == VAR(0));
                expectFailure(
                        check,
                        countMatching(intVec, VAR(equals0)) == VAR(1),
                        "(countMatching([1, 2, 3], equals0) == 1)"
                );
            });

            check.when("allAnyOrNone", [&] {
                check(allOf(constVec, VAR(equals1)));
                expectFailure(check, allOf(intVec, VAR(equals1)), "allOf([1, 2, 3], equals1)");

                check(anyOf(intVec, VAR(equals1)));
                expectFailure(check, anyOf(intVec, VAR(equals0)), "anyOf([1, 2, 3], equals0)");

                check(noneOf(intVec, VAR(equals0)));
                expectFailure(check, noneOf(intVec, VAR(equals1)), "noneOf([1, 2, 3], equals1)");
            });

            check.when("length", [&] {
                check(length(VAR(intVec)) == 3u);
                check(length(VAR(constVec)) == 4u);
                expectFailure(check, length(VAR(intVec)) == 0u, "(length(intVec) == 0)");
            });

            check.when("equalRanges", [&] {
                const vector<int> start{1, 2};
                const vector<int> last{2, 3};
                const vector<int> larger{1, 2, 3, 4};
                const vector<int> smaller{0};

                check.when("startsWith", [&] {
                    check(startsWith(intVec, intVec));
                    check(startsWith(intVec, start));
                    check(!startsWith(intVec, last));
                    check(!startsWith(intVec, larger));
                    expectFailure(check, startsWith(intVec, VAR(smaller)), "startsWith([1, 2, 3], smaller)");
                });

                check.when("endsWith", [&] {
                    check(endsWith(intVec, intVec));
                    check(!endsWith(intVec, start));
                    check(endsWith(intVec, last));
                    check(!endsWith(intVec, larger));
                    expectFailure(check, endsWith(intVec, VAR(smaller)), "endsWith([1, 2, 3], smaller)");
                });

                check.when("contains", [&] {
                    check(contains(intVec, intVec));
                    check(contains(intVec, start));
                    check(contains(intVec, last));
                    check(!contains(intVec, larger));
                    expectFailure(check, contains(intVec, VAR(smaller)), "contains([1, 2, 3], smaller)");
                });
            });
        }),
        given("ValueSemantics", [] (Check& check) {
            MoveTracker moveTracker;

            VAR(moveTracker);
            check("We don't steal the expression object", ! VAR(moveTracker.moved()));

            VAR([] (const MoveTracker&) {}) (moveTracker);
            check("We don't steal function arguments", ! VAR(moveTracker.moved()));

            // gcc sanitizers will hopefully pick up any stored refs to temporaries
            int a = 1;
            int b = 1;
            check("We don't store refs to temporaries", VAR(a + b) == 2);

            const int c = 1;
            check("Const is preserved on lvalue refs", VAR(c) == 1);

            namespace Conf = Assertion::Impl::Configurable;

            static_assert(
                    is_same<decltype(VAR(a + b)), Conf::VariableValueExpression<int>>::value,
                    "Temporaries are stored by value"
                );

            static_assert(
                    is_same<decltype(VAR(a)), Conf::VariableRefExpression<int>>::value,
                    "Variables are stored by reference"
            );

            const auto aConst = a;

            static_assert(
                    is_same<decltype(VAR(aConst)), Conf::VariableRefExpression<const int>>::value,
                    "const is preserved"
            );

            static_assert(
                is_same<
                    decltype(VAR(id)(a + b)),
                    Conf::FunctionValue<reference_wrapper<int(int)>, int>
                >::value,
                "Temporaries are stored by value"
            );

            static_assert(
                is_same<
                    decltype(VAR(id)(a)),
                    Conf::FunctionValue<reference_wrapper<int(int)>, int&>
                >::value,
                "Values are stored by reference"
            );
        }),
        given("ThrowSucceeds", [] (Check& check) {
            RecordFailures recordFailures;
            testAssertThrows<exception>(out(recordFailures), VAR([] { throw runtime_error("test"); })());
            testAssertThrows<runtime_error>(out(recordFailures), VAR([] { throw runtime_error("test"); })());

            check( ! VAR(bool(recordFailures.failure())));
        }),
        given("ThrowFails", [] (Check& check) {
            expectException<logic_error>(
                    check,
                    VAR([] { throw runtime_error("test"); })(),
                    "[] { throw runtime_error(\"test\"); }() threw \"test\""
                );

            expectException<runtime_error>(
                    check,
                    VAR([] {} )(),
                    "[] {}()"
            );

        }),
        given("BinaryBoolean", [] (Check& check) {
            testBooleanOperator<std::logical_and<void>>(check);
            testBooleanOperator<std::logical_or<void>>(check);
        }),
        given("Comparison", [] (Check& check) {
            testNumericOperator<std::equal_to<void>>(check);
            testNumericOperator<std::not_equal_to<void>>(check);
            testNumericOperator<std::less<void>>(check);
            testNumericOperator<std::less_equal<void>>(check);
            testNumericOperator<std::greater<void>>(check);
            testNumericOperator<std::greater_equal<void>>(check);
        }),
        given("Arithmetic", [] (Check& check) {
            testNumericOperator<std::plus<void>>(check);
            testNumericOperator<std::minus<void>>(check);
            testNumericOperator<std::multiplies<void>>(check);

            testNonZeroDenominator<std::modulus<void>>(check);
            testNonZeroDenominator<std::divides<void>>(check);
        }),
        given("Bitwise", [] (Check& check) {
            testNumericOperator<std::bit_and<void>>(check);
            testNumericOperator<std::bit_or<void>>(check);
            testNumericOperator<std::bit_xor<void>>(check);
        })
    );
}

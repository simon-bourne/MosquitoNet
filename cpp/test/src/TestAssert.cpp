// Copyright (C) 2014 Enhedron Ltd

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

    namespace Assertion {
        template<typename T>
        struct Convert<vector<T>> {
            static inline string toString(const vector<T>& value) {
                string result("[");
                bool isFirst = true;

                for (const auto& element : value) {
                    if ( ! isFirst) {
                        result += ", ";
                    }

                    isFirst = false;
                    result += Convert<T>::toString(element);
                }

                result += "]";

                return result;
            }
        };
    }

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

        virtual void handleCheckFailure(const string& expressionText, const vector<Variable>& variableList) override {
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

            if (check(VAL(bool(failure))) && expressionText) {
                check(VAL(failure->expressionText) == expressionText);
            }
        }
        catch (const exception& e){
            check.fail(VAL(e.what()));
        }
    }

    template<typename Operator, typename Value>
    void testBooleanOperator(Check& check, Value lhs, Value rhs) {
        Operator op;
        bool result = op(lhs, rhs);

        if (result) {
            check(op(VAL(lhs), VAL(rhs)));
            check(op(lhs, VAL(rhs)));
            check(op(VAL(lhs), rhs));
        }
        else {
            expectFailure(check, op(VAL(lhs), VAL(rhs)));
            expectFailure(check, op(lhs, VAL(rhs)));
            expectFailure(check, op(VAL(lhs), rhs));
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

        check(op(VAL(lhs), VAL(rhs)) == result);
        check(op(lhs, VAL(rhs)) == result);
        check(op(VAL(lhs), rhs) == result);
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

            if (check(VAL(bool(failure))) && expressionText) {
                check(VAL(failure->expressionText) == expressionText);
            }
        }
        catch (const exception& e) {
            check.fail(VAL(e.what()));
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

    static auto countProxy = makeFunction(
        "count",
        [] (auto&& container, auto&& value) {
            return count(container.begin(), container.end(), forward<decltype(value)>(value));
        }
    );

    int id(int x) { return x; }

    static Test::Suite s("Assert",
        given("Success", [] (Check& check) {
            check(VAL(true));
            check(! VAL(false));
            check(! VAL(false) && !VAL(false));
            check(VAL(sum3)(1, 2, 3) == 6);
        }),
        given("Failure", [] (Check& check) {
            expectFailure(check, VAL(false), "false");
            expectFailure(check, VAL(false) || VAL(false), "(false || false)");

            int a = 1;
            expectFailure(check, VAL(sum3)(VAL(a), 2, 3) == 7, "(sum3(a, 2, 3) == 7)");
        }),
        given("Overloaded", [] (Check& check) {
            check(overloadedProxy(1) == overloaded(1));
            check(overloadedProxy(1.0) == overloaded(1.0));

            int a = 1;
            expectFailure(check, overloadedProxy(VAL(a)) == 2, "(overloaded(a) == 2)");
        }),
        given("Template", [] (Check& check) {
            vector<int> intVec{ 1, 2, 3 };
            check(countProxy(intVec, 1) == VAL(1));
            check(countProxy(intVec, 0) == VAL(0));
            expectFailure(check, countProxy(intVec, 0) == VAL(1), "(count([1, 2, 3], 0) == 1)");
        }),
        given("ValueSemantics", [] (Check& check) {
            MoveTracker moveTracker;

            VAL(moveTracker);
            check("We don't steal the expression object", ! VAL(moveTracker.moved()));

            VAL([] (const MoveTracker&) {}) (moveTracker);
            check("We don't steal function arguments", ! VAL(moveTracker.moved()));

            // Need to run through sanitizers to check we don't store refs to temporaries
            int a = 1;
            int b = 1;
            check("We don't store refs to temporaries", VAL(a + b) == 2);

            const int c = 1;
            check("Const is preserved on lvalue refs", VAL(c) == 1);

            namespace Conf = Assertion::Impl::Configurable;

            static_assert(
                    is_same<decltype(VAL(a + b)), Conf::VariableValueExpression<int>>::value,
                    "Temporaries are stored by value"
                );

            static_assert(
                    is_same<decltype(VAL(a)), Conf::VariableRefExpression<int>>::value,
                    "Variables are stored by reference"
            );

            const auto aConst = a;

            static_assert(
                    is_same<decltype(VAL(aConst)), Conf::VariableRefExpression<const int>>::value,
                    "const is preserved"
            );

            static_assert(
                is_same<
                    decltype(VAL(id)(a + b)),
                    Conf::FunctionValue<reference_wrapper<int(int)>, int>
                >::value,
                "Temporaries are stored by value"
            );

            static_assert(
                is_same<
                    decltype(VAL(id)(a)),
                    Conf::FunctionValue<reference_wrapper<int(int)>, int&>
                >::value,
                "Values are stored by reference"
            );
        }),
        given("ThrowSucceeds", [] (Check& check) {
            RecordFailures recordFailures;
            testAssertThrows<exception>(out(recordFailures), VAL([] { throw runtime_error("test"); })());
            testAssertThrows<runtime_error>(out(recordFailures), VAL([] { throw runtime_error("test"); })());

            check( ! VAL(bool(recordFailures.failure())));
        }),
        given("ThrowFails", [] (Check& check) {
            expectException<logic_error>(
                    check,
                    VAL([] { throw runtime_error("test"); })(),
                    "[] { throw runtime_error(\"test\"); }() threw \"test\""
                );

            expectException<runtime_error>(
                    check,
                    VAL([] {} )(),
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

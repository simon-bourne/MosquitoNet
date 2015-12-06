//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#include "MosquitoNet.h"

#include <vector>
#include <stdexcept>
#include <utility>
#include <string>
#include <sstream>

using namespace Enhedron::Test;
using std::vector;
using std::runtime_error;
using std::forward;
using std::string;
using std::ostringstream;

// The comments in this file are used when building the documentation.

// Assertion example 1:
static Suite s("examples", context("assertion",
    given("some constants to assert with", [] (auto& check) {
        int a = 0;
        int b = 1;

        check(VAR(a) == 0 && VAR(b) == 1);
    })
));
// Assertion example 1 end.

void throwRuntimeError() {
    throw runtime_error("Expected exception");
}

// Multiply example begin.
int multiply(int x, int y) { return x * y; }
// Multiply example end.

// Assertion example 14:
template<typename Value>
Value multiplyOverloaded(Value x, Value y) {
    return x * y;
}

template<typename Value>
Value multiplyOverloaded(Value x, Value y, Value z) {
    return x * y * z;
}
// Assertion example 14 end.

// Assertion example 15:
template<typename... Args>
auto multiplyOverloadedProxy(Args&&... args) {
    return makeFunction(
            "multiplyOverloaded",
            [] (auto&&... args) { return multiplyOverloaded(forward<decltype(args)>(args)...); }
    )(forward<Args>(args)...);
}
// Assertion example 15 end.

static Suite t("examples", context("assertion",
    given("some constants to assert with", [] (auto& check) {
        int a = 0;
        int b = 1;
        int c = 2;
        int d = 4;

        // Assertion example 2:
        check("a is one and b is two", VAR(a) == 0 && VAR(b) == 1);
        // Assertion example 2 end.

        // Assertion example 3:
        check("`c` and `d` are provided for context", VAR(a) == 0 && VAR(b) == 1, VAR(c), VAR(d));
        // Assertion example 3 end.

        // Assertion example 4:
        check("a is one and b is two", VAR(a) == 0 && b == 1 && VAR(a + c) == c);
        // Assertion example 4 end.

        // Assertion example 5:
        check.throws("an exception is thrown", VAR(throwRuntimeError)());
        // Assertion example 5 end.

        // Assertion example 6:
        check. template throws<runtime_error>("a runtime_error is thrown", VAR(throwRuntimeError)());
        // Assertion example 6 end.

        // Assertion example 7:
        check.throws("`a` and `b` are provided for context", VAR(throwRuntimeError)(), VAR(a), VAR(b));
        // Assertion example 7 end.

        // Assertion example 8:
        check.fail(VAR("explicit failure"));
        // Assertion example 8 end.

        // Assertion example 9:
        check.fail(VAR("`a` and `b` are provided for context"), VAR(a), VAR(b));
        // Assertion example 9 end.

        // Assertion example 10:
        vector<int> v{1,2,3};
        check("the length of a vector is 3", length(VAR(v)) == 3u);
        // Assertion example 10 end.

        // Assertion example 12:
        check("3 squared is 9", VAR(multiply) (3, 3) == 9);
        // Assertion example 12 end.

        // Assertion example 13:
        int three = 3;
        check("3 squared is 9", VAR(multiply) (VAR(three), 3) == 9);
        // Assertion example 13 end.

        // Assertion example 16:
        check("3 cubed is 27", multiplyOverloadedProxy(VAR(three), 3, 3) == 27);
        // Assertion example 16 end.
    })
));

// Writing tests: contexts begin.
static constexpr const int a = 1;

static Suite u("the root context",
    given("a test can go here", [] (auto& check) {
        check("`a` is one", VAR(a) == 1);
    }),
    context("a context",
        given("or a test can go here", [] (auto& check) {
            check("`a` is one", VAR(a) == 1);
        }),
        context("contexts can be nested to arbitrary depth",
            given("or a test can go here", [] (auto& check) {
                check("`a` is one", VAR(a) == 1);
            })
        )
    )
);
// Writing tests: contexts end.

// Parameterized multiply test begin.
void testMultiply(Check& check, int x, int y, int expectedResult) {
    check.when("we multiply them together with `multiply`", [&] {
        check(VAR(multiply) (x, y) == expectedResult);
    });
}
// Parameterized multiply test end.

// Pretty print define begin.
struct MyType {
    int x;
    int y;
};

namespace Enhedron { namespace Assertion {
    template<>
    struct Convert<MyType> {
        static string toString(const MyType& value) {
            ostringstream output;
            output << "{ x = " << value.x << ", y = " << value.y << " }";
            return output.str();
        }
    };
}}
// Pretty print define end.

static Suite v("writing tests",
    // Writing tests: basic when begin.
    given("a variable `a = 0`", [] (auto& check) {
        int a = 0;

        check.when("we add 1 to it", [&] {
            a += 1;
            check(VAR(a) == 1);
        });

        check.when("we add 2 to it", [&] {
            a += 2;
            check(VAR(a) == 2);
        });

        check.when("we add 3 to it", [&] {
            a += 3;
            check(VAR(a) == 3);
        });
    }),
    // Writing tests: basic when end.

    // Writing tests: nested when begin.
    given("a variable `a = 0`", [] (auto& check) {
        int a = 0;

        check.when("we add 1 to it", [&] {
            a += 1;
            check(VAR(a) == 1);

            check.when("we add 10 to it", [&] {
                a += 10;
                check(VAR(a) == 11);
            });

            check.when("we add 20 to it", [&] {
                a += 20;
                check(VAR(a) == 21);
            });
        });

        check.when("we add 2 to it", [&] {
            a += 2;
            check(VAR(a) == 2);
        });

        check.when("we add 3 to it", [&] {
            a += 3;
            check(VAR(a) == 3);
        });
    }),
    // Writing tests: nested when end.

    // Parameterized multiply begin.
    given("0 and 0",   testMultiply, 0, 0, 0),
    given("0 and 1",   testMultiply, 0, 1, 0),
    given("0 and 10",  testMultiply, 0, 10, 0),
    given("1 and 0",   testMultiply, 1, 0, 0),
    given("1 and 1",   testMultiply, 1, 1, 1),
    given("1 and 10",  testMultiply, 1, 10, 10),
    given("10 and 0",  testMultiply, 10, 0, 0),
    given("10 and 1",  testMultiply, 10, 1, 10),
    given("10 and 10", testMultiply, 10, 10, 100),
    // Parameterized multiply end.

    // Model check multiply begin.
    exhaustive(choice(0, 1, 10), choice(0, 1, 10)).
        given("2 numbers", [] (Check& check, int x, int y) {
            check.when("we multiply them together", [&] {
                int result = multiply(x, y);

                // This is our model to check multiply against.
                int expected = 0;

                for (int i = 0; i < x; ++i) {
                    expected += y;
                }

                check(VAR(result) == VAR(expected), VAR(x), VAR(y));
            });
        }
    ),
    // Model check multiply end.

    // Pretty print test begin.
    given("an instance of MyType", [] (auto& check) {
        MyType myType{1, 2};
        check(VAR(myType.x) == 1, VAR(myType));
    })
    // Pretty print test end.
);

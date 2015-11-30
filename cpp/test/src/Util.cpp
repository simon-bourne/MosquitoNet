//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#include "Enhedron/Test.h"
#include "Enhedron/Util/Enum.h"
#include "Enhedron/Util/Json.h"
#include "Enhedron/Util/Math.h"

#include <string>
#include <utility>

namespace Enhedron {
    using std::string;
    using std::pair;
    using std::make_pair;

    using namespace Test;
    using namespace Util;

    static constexpr const char helloWorld[] = "Hello World!";

    enum class SimpleEnum {
        VALUE_0,
        VALUE_1,
        LAST_ENUM_VALUE = SimpleEnum::VALUE_1
    };

    static const auto escapedPairs = choice(
            make_pair('\"', '\"'),
            make_pair('\\', '\\'),
            make_pair('\b', 'b'),
            make_pair('\f', 'f'),
            make_pair('\n', 'n'),
            make_pair('\r', 'r'),
            make_pair('\t', 't')
    );

    static Test::Suite u("Util",
        given("out", [] (Check& check) {
            int value = 10;
            auto outParam(out(value));
            *outParam *= 2;

            check(VAR(value) == 20);
            check(VAR(*outParam) == value);
            check(VAR(outParam.get()) == value);
        }),
        given("taggedValue", [] (Check& check) {
            TaggedValue<SimpleEnum, SimpleEnum::VALUE_0, string> value(helloWorld);

            check(VAR(*value) == helloWorld);
            check(VAR(value->size()) == string(helloWorld).size());

            *value += "!";

            check(VAR(value->size()) == string(helloWorld).size() + 1);
        }),
        given("toEnum", [] (Check& check) {
            check(VAR(toEnum<SimpleEnum>(0)) == SimpleEnum::VALUE_0);
            check(VAR(toEnum<SimpleEnum>(1)) == SimpleEnum::VALUE_1);
            check.throws(VAR([] { toEnum<SimpleEnum>(-1); } ) ());
            check.throws(VAR([] { toEnum<SimpleEnum>(2); } ) ());
        }),
        given("jsonEscape", [] (Check& check) {
            check(VAR(jsonEscape("")) == "");
            check(VAR(jsonEscape)(helloWorld) == VAR(helloWorld));
            check(VAR(jsonEscape("prefix\u0001postfix")) == "prefix\\u0001postfix");
            check(VAR(jsonEscape("prefix\u0010postfix")) == "prefix\\u0010postfix");
            check(VAR(jsonEscape("prefix\tpostfix")) == "prefix\\tpostfix");
        }),
        exhaustive(
                escapedPairs,
                choice("", "prefix"),
                choice("", "postfix")
            ).
            given("a prefix, a postfix and a char that requires escaping", [] (
                        Check& check,
                        pair<char, char> c,
                        const string& prefix,
                        const string& postfix
                    )
                {
                    string input(prefix);
                    input += c.first;
                    input += postfix;

                    string result(prefix);
                    result += '\\';
                    result += c.second;
                    result += postfix;

                    check.when("we escape the input", [&] {
                        check("the char is escaped", VAR(jsonEscape(input)) == result);
                    });
                }
        ),
        context("math",
            given("divideRoundingUp", [] (Check& check) {
                check(VAR(divideRoundingUp(0u, 10u)) == 0u);
                check(VAR(divideRoundingUp(1u, 10u)) == 1u);
                check(VAR(divideRoundingUp(10u, 10u)) == 1u);
                check(VAR(divideRoundingUp(11u, 10u)) == 2u);
            }),
            given("makeDivisibleByRoundingDown", [] (Check& check) {
                for (auto i = 1u; i < 10u; ++i) {
                    check(VAR(makeDivisibleByRoundingDown(0u, i)) == 0u);
                }

                check(VAR(makeDivisibleByRoundingDown(10u, 1u)) == 10u);
                check(VAR(makeDivisibleByRoundingDown(10u, 2u)) == 10u);
                check(VAR(makeDivisibleByRoundingDown(10u, 3u)) == 9u);
                check(VAR(makeDivisibleByRoundingDown(10u, 4u)) == 8u);
                check(VAR(makeDivisibleByRoundingDown(10u, 5u)) == 10u);
            }),
            given("makeDivisibleByRoundingUp", [] (Check& check) {
                for (auto i = 1u; i < 10u; ++i) {
                    check(VAR(makeDivisibleByRoundingUp(0u, i)) == 0u);
                }

                check(VAR(makeDivisibleByRoundingUp(10u, 1u)) == 10u);
                check(VAR(makeDivisibleByRoundingUp(10u, 2u)) == 10u);
                check(VAR(makeDivisibleByRoundingUp(10u, 3u)) == 12u);
                check(VAR(makeDivisibleByRoundingUp(10u, 4u)) == 12u);
                check(VAR(makeDivisibleByRoundingUp(10u, 5u)) == 10u);
            })
        )
    );
}

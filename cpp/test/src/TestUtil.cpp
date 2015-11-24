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

            check(VAL(value) == 20);
            check(VAL(*outParam) == value);
            check(VAL(outParam.get()) == value);
        }),
        given("taggedValue", [] (Check& check) {
            TaggedValue<SimpleEnum, SimpleEnum::VALUE_0, string> value(helloWorld);

            check(VAL(*value) == helloWorld);
            check(VAL(value->size()) == string(helloWorld).size());

            *value += "!";

            check(VAL(value->size()) == string(helloWorld).size() + 1);
        }),
        given("toEnum", [] (Check& check) {
            check(VAL(toEnum<SimpleEnum>(0)) == SimpleEnum::VALUE_0);
            check(VAL(toEnum<SimpleEnum>(1)) == SimpleEnum::VALUE_1);
            check.throws(VAL([] { toEnum<SimpleEnum>(-1); } ) ());
            check.throws(VAL([] { toEnum<SimpleEnum>(2); } ) ());
        }),
        given("jsonEscape", [] (Check& check) {
            check(VAL(jsonEscape("")) == "");
            check(VAL(jsonEscape)(helloWorld) == VAL(helloWorld));
            check(VAL(jsonEscape("prefix\u0001postfix")) == "prefix\\u0001postfix");
            check(VAL(jsonEscape("prefix\u0010postfix")) == "prefix\\u0010postfix");
            check(VAL(jsonEscape("prefix\tpostfix")) == "prefix\\tpostfix");
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
                        check("the char is escaped", VAL(jsonEscape(input)) == result);
                    });
                }
        ),
        context("math",
            given("divideRoundingUp", [] (Check& check) {
                check(VAL(divideRoundingUp(0u, 10u)) == 0u);
                check(VAL(divideRoundingUp(1u, 10u)) == 1u);
                check(VAL(divideRoundingUp(10u, 10u)) == 1u);
                check(VAL(divideRoundingUp(11u, 10u)) == 2u);
            }),
            given("makeDivisibleByRoundingDown", [] (Check& check) {
                for (auto i = 1u; i < 10u; ++i) {
                    check(VAL(makeDivisibleByRoundingDown(0u, i)) == 0u);
                }

                check(VAL(makeDivisibleByRoundingDown(10u, 1u)) == 10u);
                check(VAL(makeDivisibleByRoundingDown(10u, 2u)) == 10u);
                check(VAL(makeDivisibleByRoundingDown(10u, 3u)) == 9u);
                check(VAL(makeDivisibleByRoundingDown(10u, 4u)) == 8u);
                check(VAL(makeDivisibleByRoundingDown(10u, 5u)) == 10u);
            }),
            given("makeDivisibleByRoundingUp", [] (Check& check) {
                for (auto i = 1u; i < 10u; ++i) {
                    check(VAL(makeDivisibleByRoundingUp(0u, i)) == 0u);
                }

                check(VAL(makeDivisibleByRoundingUp(10u, 1u)) == 10u);
                check(VAL(makeDivisibleByRoundingUp(10u, 2u)) == 10u);
                check(VAL(makeDivisibleByRoundingUp(10u, 3u)) == 12u);
                check(VAL(makeDivisibleByRoundingUp(10u, 4u)) == 12u);
                check(VAL(makeDivisibleByRoundingUp(10u, 5u)) == 10u);
            })
        )
    );
}

#include "Enhedron/Test.h"
#include "Enhedron/Util/Enum.h"
#include "Enhedron/Util/Json.h"
#include "Enhedron/Util/Math.h"

namespace Enhedron {
    using std::string;

    using namespace Test;
    using namespace Util;

    static constexpr const char helloWorld[] = "Hello World!";

    enum class SimpleEnum {
        VALUE_0,
        VALUE_1,
        LAST_ENUM_VALUE = SimpleEnum::VALUE_1
    };

    static Test::Suite u("Util",
        simple("out", [] (Check& check) {
            int value = 10;
            auto outParam(out(value));
            *outParam *= 2;

            check(VAL(value) == 20);
            check(VAL(*outParam) == value);
            check(VAL(outParam.get()) == value);
        }),
        simple("taggedValue", [] (Check& check) {
            TaggedValue<SimpleEnum, SimpleEnum::VALUE_0, string> value(helloWorld);

            check(VAL(*value) == helloWorld);
            check(VAL(value->size()) == string(helloWorld).size());

            *value += "!";

            check(VAL(value->size()) == string(helloWorld).size() + 1);
        }),
        simple("toEnum", [] (Check& check) {
            check(VAL(toEnum<SimpleEnum>(0)) == SimpleEnum::VALUE_0);
            check(VAL(toEnum<SimpleEnum>(1)) == SimpleEnum::VALUE_1);
            check.throws(VAL([] { toEnum<SimpleEnum>(-1); } ) ());
            check.throws(VAL([] { toEnum<SimpleEnum>(2); } ) ());
        }),
        simple("jsonEscape", [] (Check& check) {
            check(VAL(jsonEscape("")) == "");
            check(VAL(jsonEscape)(helloWorld) == VAL(helloWorld));
            check(VAL(jsonEscape("prefix\u0001postfix")) == "prefix\\u0001postfix");
            check(VAL(jsonEscape("prefix\u0010postfix")) == "prefix\\u0010postfix");
        }),
        exhaustive(
                choice('\"', '\\', '\b', '\f', '\n', '\r', '\t'),
                choice("", "prefix"),
                choice("", "postfix")
            ).
            simple("jsonEscapingCombo", [] (Check& check, char c, const string& prefix, const string& postfix) {
                string input(prefix);
                input += c;
                input += postfix;

                string result(prefix);
                result += '\\';
                result += c;
                result += postfix;

                check(VAL(jsonEscape(input)) == result);
            }
        ),
        context("math",
            simple("divideRoundingUp", [] (Check& check) {
                check(VAL(divideRoundingUp(0u, 10u)) == 0u);
                check(VAL(divideRoundingUp(1u, 10u)) == 1u);
                check(VAL(divideRoundingUp(10u, 10u)) == 1u);
                check(VAL(divideRoundingUp(11u, 10u)) == 2u);
            }),
            simple("makeDivisibleByRoundingDown", [] (Check& check) {
                for (auto i = 1u; i < 10u; ++i) {
                    check(VAL(makeDivisibleByRoundingDown(0u, i)) == 0u);
                }

                check(VAL(makeDivisibleByRoundingDown(10u, 1u)) == 10u);
                check(VAL(makeDivisibleByRoundingDown(10u, 2u)) == 10u);
                check(VAL(makeDivisibleByRoundingDown(10u, 3u)) == 9u);
                check(VAL(makeDivisibleByRoundingDown(10u, 4u)) == 8u);
                check(VAL(makeDivisibleByRoundingDown(10u, 5u)) == 10u);
            }),
            simple("makeDivisibleByRoundingUp", [] (Check& check) {
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

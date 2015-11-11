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

    static Test::Unit u(context("Util",
        simple("out", [] (Check& check) {
            int value = 10;
            auto outParam(out(value));
            *outParam *= 2;

            check(M_VAR(value) == 20);
            check(M_VAR(*outParam) == value);
            check(M_VAR(outParam.get()) == value);
        }),
        simple("taggedValue", [] (Check& check) {
            TaggedValue<SimpleEnum, SimpleEnum::VALUE_0, string> value(helloWorld);

            check(M_VAR(*value) == helloWorld);
            check(M_VAR(value->size()) == string(helloWorld).size());

            *value += "!";

            check(M_VAR(value->size()) == string(helloWorld).size() + 1);
        }),
        simple("toEnum", [] (Check& check) {
            check(M_VAR(toEnum<SimpleEnum>(0)) == SimpleEnum::VALUE_0);
            check(M_VAR(toEnum<SimpleEnum>(1)) == SimpleEnum::VALUE_1);
            check.throws(M_VOID(toEnum<SimpleEnum>(-1)));
            check.throws(M_VOID(toEnum<SimpleEnum>(2)));
        }),
        simple("jsonEscape", [] (Check& check) {
            check(M_VAR(jsonEscape("")) == "");
            check(M_VAR(jsonEscape(helloWorld)) == helloWorld);
            check(M_VAR(jsonEscape("prefix\u0001postfix")) == "prefix\\u0001postfix");
            check(M_VAR(jsonEscape("prefix\u0010postfix")) == "prefix\\u0010postfix");
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

                check(M_VAR(jsonEscape(input)) == result);
            }
        ),
        context("math",
            simple("divideRoundingUp", [] (Check& check) {
                check(M_VAR(divideRoundingUp(0u, 10u)) == 0u);
                check(M_VAR(divideRoundingUp(1u, 10u)) == 1u);
                check(M_VAR(divideRoundingUp(10u, 10u)) == 1u);
                check(M_VAR(divideRoundingUp(11u, 10u)) == 2u);
            }),
            simple("makeDivisibleByRoundingDown", [] (Check& check) {
                for (auto i = 1u; i < 10u; ++i) {
                    check(M_VAR(makeDivisibleByRoundingDown(0u, i)) == 0u);
                }

                check(M_VAR(makeDivisibleByRoundingDown(10u, 1u)) == 10u);
                check(M_VAR(makeDivisibleByRoundingDown(10u, 2u)) == 10u);
                check(M_VAR(makeDivisibleByRoundingDown(10u, 3u)) == 9u);
                check(M_VAR(makeDivisibleByRoundingDown(10u, 4u)) == 8u);
                check(M_VAR(makeDivisibleByRoundingDown(10u, 5u)) == 10u);
            }),
            simple("makeDivisibleByRoundingUp", [] (Check& check) {
                for (auto i = 1u; i < 10u; ++i) {
                    check(M_VAR(makeDivisibleByRoundingUp(0u, i)) == 0u);
                }

                check(M_VAR(makeDivisibleByRoundingUp(10u, 1u)) == 10u);
                check(M_VAR(makeDivisibleByRoundingUp(10u, 2u)) == 10u);
                check(M_VAR(makeDivisibleByRoundingUp(10u, 3u)) == 12u);
                check(M_VAR(makeDivisibleByRoundingUp(10u, 4u)) == 12u);
                check(M_VAR(makeDivisibleByRoundingUp(10u, 5u)) == 10u);
            })
        )
    ));
}

#include "Enhedron/Test.h"

namespace Enhedron {
    using std::array;
    using namespace Test;

    static Test::Suite u("Integration",
        given("empty", [] (Check& check) {
        }),
        given("simple", [] (Check& check) {
            check(VAL(1) == 1);
        }),
        given("simpleWhen", [] (Check& check) {
            check.when("when", [] {
            });
        }),
        given("complexWhen", [] (Check& check) {
            // The numbers should come out in order, and should have a "fresh check" before each one.
            check("Fresh", VAL(true));
            check.when("b", [&] {
                check.when("c", [&]{
                    check(">>> 1", VAL(true));
                });

                check.when("d", [&]{
                    check.when("e", [&]{
                        check(">>> 2", VAL(true));
                    });
                });
            });

            check.when("f", [&]{
                check(">>> 3", VAL(true));
            });

            check.when("g", [&]{
                check.when("g", [&]{
                    check(">>> 4", VAL(true));
                });
            });
        }),
        given("simpleWhen", [] (Check& check) {
            check("penultimate failure", VAL(false));

            check.when("when", [&] {
                check("last failure", VAL(false));
            });
        })
    );
}

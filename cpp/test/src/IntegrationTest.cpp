//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#include "Enhedron/Test.h"

namespace Enhedron {
    using std::array;
    using namespace Test;

    static Test::Suite u("Integration",
        given("empty", [] (Check& check) {
        }),
        given("simple", [] (Check& check) {
            check(VAR(1) == 1);
        }),
        given("simpleWhen", [] (Check& check) {
            check.when("when", [] {
            });
        }),
        given("complexWhen", [] (Check& check) {
            // The numbers should come out in order, and should have a "fresh check" before each one.
            check("Fresh", VAR(true));
            check.when("b", [&] {
                check.when("c", [&]{
                    check(">>> 1", VAR(true));
                });

                check.when("d", [&]{
                    check.when("e", [&]{
                        check(">>> 2", VAR(true));
                    });
                });
            });

            check.when("f", [&]{
                check(">>> 3", VAR(true));
            });

            check.when("g", [&]{
                check.when("g", [&]{
                    check(">>> 4", VAR(true));
                });
            });
        }),
        given("simpleWhen", [] (Check& check) {
            check("penultimate failure", VAR(false));

            check.when("when", [&] {
                check("last failure", VAR(false));
            });
        })
    );
}

//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#include "MosquitoNet.h"

namespace Enhedron {
    using namespace Test;

    static Test::Suite u("SingleInclude",
        given("empty", [] (Check& check) {
        }),
        given("simple", [] (Check& check) {
            check(VAR(1) == 1);
        }),
        given("simpleWhen", [] (Check& check) {
            check.when("when", [] {
            });
        })
    );
}

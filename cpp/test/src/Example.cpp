//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#include "MosquitoNet.h"

#include <vector>
#include <set>

using namespace Enhedron::Test;
using std::vector;
using std::set;

void checkVectorSize(Check& check, size_t size) {
    vector<int> v(size, 0);
    check(length(VAR(v)) == size);
}

static Suite u("Util",
    given("a very simple test", [] (auto& check) {
        int a = 1;

        // VAR is the only macro we need. If the name clashes, undef it
        // and use M_ENHEDRON_VAR. Upon failure, this will log "a == 1",
        // along with the value of `a`.
        check(VAR(a) == 1);
    }),

    given("an empty set", [] (auto& check) {
        set<int> s;

        // Upon failure, this will log "length(theSet) == 1", along with
        // the contents of the set.
        check("it is initially empty", length(VAR(s)) == 0u);

        check.when("we add an element", [&] {
            s.insert(1);
            check("the size is 1", length(VAR(s)) == 1u);

            // The whole test will be run afresh for each path through the
            // when blocks. In this case twice.
            check.when("we add a different element, the size is 2", [&] {
                s.insert(2);
                check("the size is still 1", length(VAR(s)) == 2u);
            });

            check.when("we add the same element, the size is 1", [&] {
                s.insert(1);
                check("the size is still 1", length(VAR(s)) == 1u);
            });
        });
    }),

    // Parameterized tests:
    given("a vector of size 0", checkVectorSize, 0),
    given("a vector of size 10", checkVectorSize, 10),

    // Model checking.
    exhaustive(
            choice(0, 10, 20), // These are the values for `initialSize`.
            choice(0, 5, 10, 15, 20, 25) // and these are for `resizeTo`.
        ).
        given("a vector with some elements", [] (
                    Check& check,
                    size_t initialSize,
                    size_t resizeTo
                )
            {
                vector<int> v(initialSize, 0);
                check("the initial size is correct",
                      length(VAR(v)) == initialSize);

                check.when("we resize it", [&] {
                    v.resize(resizeTo);
                    check("the new size is correct",
                          length(VAR(v)) == resizeTo);

                    check("the size <= the capacity",
                          length(VAR(v)) <= v.capacity());
                });
            }
    ),

    context("we can also nest contexts",
        context("to an arbitrary depth",
            given("an empty test to illustrate nesting", [] (auto& check) {
            })
        )
    )
);

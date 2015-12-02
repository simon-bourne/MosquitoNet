Getting Started
===============

Download the single header latest release of the single header, `MosquitoNet.h` from
`here <https://raw.githubusercontent.com/simon-bourne/MosquitoNet/stable/cpp/single-include/MosquitoNet.h>`_.
In the same directory as `MosquitoNet.h`, create a file `TestHarness.cpp` with this code in it:

.. code-block:: c++

    #include "MosquitoNet.h"

    int main(int argc, const char* argv[]) {
        return ::Enhedron::Test::run(argc, argv);
    }

Then compile it with `g++` (version 5 or later, but 4.9 will work with `--std=c++1y`):

.. code-block:: none

    g++ --std=c++14 -o test-harness TestHarness.cpp

Now run *./test-harness* and you should get this output:

.. code-block:: none

    Totals: 0 tests, 0 checks, 0 fixtures

Let's add a simple test. We'll check that the *empty()* method on a default constructed *vector<int>* gives *true*.
Edit your *TestHarness.cpp* so it contains:

.. code-block:: c++

    #include "MosquitoNet.h"

    #include <vector>

    using namespace Enhedron::Test;
    using std::vector;

    static Suite u("Util",
        given("a very simple test", [] (auto& check) {
            int a = 1;

            check(VAR(a) == 1);
        })
    );

    int main(int argc, const char* argv[]) {
        return run(argc, argv);
    }

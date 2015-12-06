Assertions
==========

Basic Assertions
----------------

When your test is run, it is passed a *Check* object which is your interface to MosquitoNet within the test. *Check*
overrides *operator()* to provide basic checking. We'll concentrate on that for now. The test will continue even if any
checks fail. The *VAR* macro indicates that we're interested in the value and name of a particular variable. Each
check will return a `bool` with `true` iff the check passes.

This example will check the values of *a* and *b*.

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 1:
   :end-before: // Assertion example 1 end.

When we run the test harness with *--verbosity variables* it will print out each check along with the value of the
variables in that expression:

.. code-block:: none

    examples/assertion
        Given: some constants to assert with
        Then : ((a == 0) && (b == 1))
                a = 0: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 16.
                b = 1: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 16.

Assertions can also have a description:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 2:
   :end-before: // Assertion example 2 end.
   :dedent: 8

Will output:

.. code-block:: none

    Then : a is one and b is two
        ((a == 0) && (b == 1))
            a = 0: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 27.
            b = 1: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 27.

For a less technical spec, we can suppress the expression (when a description is provided) and the variables with
*--verbosity checks*:

.. code-block:: none

    examples/assertion
        Given: some constants to assert with
        Then : a is one and b is two

We can also provide context variables that are not checked, but provide context in the output:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 3:
   :end-before: // Assertion example 3 end.
   :dedent: 8

Outputs:

.. code-block:: none

    Then : `c` and `d` are provided for context
        ((a == 0) && (b == 1))
            a = 0: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 33.
            b = 1: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 33.
            c = 2: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 33.
            d = 4: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 33.

Also note that you can use an expression inside the VAR macro and all variables don't have to be inside the VAR macro:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 4:
   :end-before: // Assertion example 4 end.
   :dedent: 8

Outputs:

.. code-block:: none

    Then : a is one and b is two
        (((a == 0) && true) && (a + c == 2))
            a = 0: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 37.
            a + c = 2: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 37.

Exceptions
----------

This will check something derived from *std::exception* is thrown:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 5:
   :end-before: // Assertion example 5 end.
   :dedent: 8

This will check something derived from *runtime_error* is thrown:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 6:
   :end-before: // Assertion example 6 end.
   :dedent: 8

Again, we can provide context variables:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 7:
   :end-before: // Assertion example 7 end.
   :dedent: 8

Explicit Failures
-----------------

We can explicitly fail by throwing an exception. We can also use the `Check` object if we'd like to continue the test:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 8:
   :end-before: // Assertion example 8 end.
   :dedent: 8

And with context variables:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 9:
   :end-before: // Assertion example 9 end.
   :dedent: 8

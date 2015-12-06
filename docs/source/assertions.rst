Assertions
==========

For the full code used here, see :doc:`sourceCodeForExamples`.

Basic Assertions
----------------

When your test is run, it is passed a *Check* object which is your interface to MosquitoNet within the test. *Check*
overrides *operator()* to provide basic checking. We'll concentrate on that for now. The test will continue even if any
checks fail. The *VAR* macro indicates that we're interested in the value and name of a particular variable. Each
check will return *true* if the check passes, *false* otherwise.

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

This will check *std::exception* or something derived from it is thrown:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 5:
   :end-before: // Assertion example 5 end.
   :dedent: 8

This will check *runtime_error* or something derived from it is thrown:

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

Containers
----------

MosquitoNet provides some utility functions for working with containers. For example, you can check the length of a
container:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 10:
   :end-before: // Assertion example 10 end.
   :dedent: 8

and it will output:

.. code-block:: none

    Then : the length of a vector is 3
        (length(v) == 3)
            v = [1, 2, 3]: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 96.

Other functions are:

  * *countEqual(container, value)* gives the number of elements equal to *value*.
  * *countMatching(container, predicate)* gives the number of elements matching *predicate*.
  * *allOf(container, predicate)* is true iff all elements match *predicate*.
  * *anyOf(container, predicate)* is true iff any elements match *predicate*.
  * *noneOf(container, predicate)* is true iff no elements match *predicate*.
  * *length(container)* gives the length of *container*.
  * *startsWith(container, prefixContainer)* is true iff *container* starts with *prefixContainer*.
  * *endWith(container, postfixContainer)* is true iff *container* ends with *postfixContainer*.
  * *contains(container, subSequenceContainer)* is true iff *subSequenceContainer* is a sub-sequence of *container*.

Customizing Assertions
----------------------

You can customize assertions to use your own functions. If you have a non-overloaded function and you don't need
template argument deduction, you can just use the function as-is in assertions. For example, given:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Multiply example begin.
   :end-before: // Multiply example end.

*multiply* can be used directly in assertions:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 12:
   :end-before: // Assertion example 12 end.
   :dedent: 8

Arguments can be recorded as variables if required:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 13:
   :end-before: // Assertion example 13 end.
   :dedent: 8

If you have an overloaded function or require template argument deduction, or just want a cleaner syntax, you can
provide a wrapper for your function. For example, given:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 14:
   :end-before: // Assertion example 14 end.

You can provide the wrapper:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 15:
   :end-before: // Assertion example 15 end.

then the check:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Assertion example 16:
   :end-before: // Assertion example 16 end.
   :dedent: 8

will output:

.. code-block:: none

    Then : 3 cubed is 27
        (multiplyOverloaded(three, 3, 3) == 27)
            three = 3: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 104.

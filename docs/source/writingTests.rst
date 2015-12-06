Writing Tests
=============

Contexts
--------

Tests can live inside contexts, which can be nested to an arbitrary depth. This example illustrates using contexts:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Writing tests: contexts begin.
   :end-before: // Writing tests: contexts end.

and, with *verbosity checks*, will output:

.. code-block:: none

    the root context
        Given: a test can go here
        Then : `a` is one

    the root context/a context
        Given: or a test can go here
        Then : `a` is one

    the root context/a context/contexts can be nested to arbitrary depth
        Given: or a test can go here
        Then : `a` is one

When Blocks
-----------

A test can have a number of *when* blocks inside it. The test is run once for each *when* block inside it, or just once
if there are no *when* blocks.

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Writing tests: basic when begin.
   :end-before: // Writing tests: basic when end.
   :dedent: 4

This will output:

.. code-block:: none

    Given: a variable `a = 0`
    When : we add 1 to it
    Then : (a == 1)

    Given: a variable `a = 0`
    When : we add 2 to it
    Then : (a == 2)

    Given: a variable `a = 0`
    When : we add 3 to it
    Then : (a == 3)

A when block can also have nested when blocks. The test is run freshly for each nested when block as well. For example
this test will run 4 times:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Writing tests: nested when begin.
   :end-before: // Writing tests: nested when end.
   :dedent: 4

You can see the individual runs of the test and the flow of control from the output:

.. code-block:: none

    Given: a variable `a = 0`
    When : we add 1 to it
    Then : (a == 1)
        When : we add 10 to it
        Then : (a == 11)

    Given: a variable `a = 0`
    When : we add 1 to it
    Then : (a == 1)
        When : we add 20 to it
        Then : (a == 21)

    Given: a variable `a = 0`
    When : we add 2 to it
    Then : (a == 2)

    Given: a variable `a = 0`
    When : we add 3 to it
    Then : (a == 3)

Parameterized Tests
-------------------

Sometimes you want to write several very similar tests. Lets test a function multiply, that just multiplies its
arguments together:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Multiply example begin.
   :end-before: // Multiply example end.

We can test several parameters with the following test:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Parameterized multiply test begin.
   :end-before: // Parameterized multiply test end.

And to register all the tests for every combination of 0, 1 and 10 on each arguments:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Parameterized multiply begin.
   :end-before: // Parameterized multiply end.

which will output:

.. code-block:: none

    Given: 0 and 0
    When : we multiply them together with `multiply`
    Then : (multiply(0, 0) == 0)

    Given: 0 and 1
    When : we multiply them together with `multiply`
    Then : (multiply(0, 1) == 0)

    Given: 0 and 10
    When : we multiply them together with `multiply`
    Then : (multiply(0, 10) == 0)

    Given: 1 and 0
    When : we multiply them together with `multiply`
    Then : (multiply(1, 0) == 0)

    Given: 1 and 1
    When : we multiply them together with `multiply`
    Then : (multiply(1, 1) == 1)

    Given: 1 and 10
    When : we multiply them together with `multiply`
    Then : (multiply(1, 10) == 10)

    Given: 10 and 0
    When : we multiply them together with `multiply`
    Then : (multiply(10, 0) == 0)

    Given: 10 and 1
    When : we multiply them together with `multiply`
    Then : (multiply(10, 1) == 10)

    Given: 10 and 10
    When : we multiply them together with `multiply`
    Then : (multiply(10, 10) == 100)

Model Checking
--------------

Sometimes we want to check against a model. Lets test *multiply* with a model that says *x * y* is the same as adding
*y* to *0*, *x* times. This will run the test for every possible combination of *choice(0, 1, 10)* on *x* and *y*:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Model check multiply begin.
   :end-before: // Model check multiply end.

The function *choice* is a wrapper around *vector*. It infers the type from it's first argument and creates the vector.
We could use any stl container with forward iterators, like *boost::irange* for example.

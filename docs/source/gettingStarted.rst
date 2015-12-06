Getting Started
===============

Download the single header latest release of the single header, *MosquitoNet.h* from
`here <https://raw.githubusercontent.com/simon-bourne/MosquitoNet/stable/cpp/single-include/MosquitoNet.h>`_.
Everything you'll need is in the *Enhedron::Test* namespace.
In the same directory as *MosquitoNet.h*, create a file *Harness.cpp* with this code in it:

.. literalinclude:: ../examples/Harness.cpp
   :language: c++

Then compile it with `g++` (version 5 or later, but 4.9 will work with `--std=c++1y`):

.. code-block:: none

    g++ --std=c++14 -o test-harness Harness.cpp

Now run *./test-harness* and you should get this output:

.. code-block:: none

    Totals: 0 tests, 0 checks, 0 fixtures

Let's add a simple test. We'll just check the value of a variable.
In the same directory again, create a file *MinimalTest.cpp* so it contains:

.. literalinclude:: ../examples/MinimalTest.cpp
   :language: c++

and compile it with:

.. code-block:: none

    g++ --std=c++14 -o test-harness MinimalTest.cpp Harness.cpp

From now on, we'll give examples as code snippets from :doc:`sourceCodeForExamples`.

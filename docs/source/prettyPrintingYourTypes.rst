Pretty Printing Your Types
==========================

You can either provide an overloaded *operato<<* or specialize *Enhedron::Assertion::Convert*:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Pretty print define begin.
   :end-before: // Pretty print define end.

The output of this:

.. literalinclude:: ../examples/AllExamples.cpp
   :language: c++
   :start-after: // Pretty print test begin.
   :end-before: // Pretty print test end.
   :dedent: 4

will now look like:

.. code-block:: none

    Then : (myType.x == 1)
            myType.x = 1: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 258.
            myType = { x = 1, y = 2 }: file "/work/build/MosquitoNet/cpp/test/src/Examples/AllExamples.cpp", line 258.

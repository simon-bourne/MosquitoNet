Introduction
============

MosquitoNet is a C++14 unit testing and model checking framework for Linux, OS X and Windows.

Features
--------

  * Single header version makes it simple to get started.
  * Only one macro, which only adds file and line arguments to a simple function call. This means you're always dealing
    with core C++ code, so you could generate tests programatically.
  * Simple tests or BDD style tests.
  * Parameterized tests.
  * Model checking. Specify a model and the values each argument can have and MosquitoNet will check every combination
    of arguments against your model.
  * Customizable assertions using natural C++ expressions.
  * Tests can continue to run after the first failure.
  * Nested test contexts.

Example
-------

This example shows most of the features of MosquitoNet. For a gentler introduction, see :doc:`gettingStarted`.

.. literalinclude:: ../examples/Introductory.cpp
   :language: c++

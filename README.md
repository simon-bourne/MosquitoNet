# CppUnitTest

[![Build Status](https://travis-ci.org/simon-bourne/CppUnitTest.png)](https://travis-ci.org/simon-bourne/CppUnitTest)

## This project is still in it's infancy!
 
I'm using it and it's working well for me, but if there's something you don't like or want, please log an issue or reply
with +1 to an existing issue.

## Features

- Exhaustive tests. You specify what values each parameter can have and your test will be run for every possible 
combination of these values.      

- Nested contexts.

- Tests will run through all checks and report all errors.

- Minimal use of macros. They're are all prefixed with M_ so you know if you're in C++ land or crazy macro land.

## Motivating Examples

We'll start with the coolest feature to hopefully inspire you to continue.
We're testing a function that escapes json strings, and we think that escape codes at the start, middle and end of
the string will be corner cases. We also want to test each individual escape code. This code will exhaustively
test every combination of these arguments.

```C++
static Test::Unit u(context("Util",
    exhaustive(
            choice('\"', '\\', '\b', '\f', '\n', '\r', '\t'),
            choice("", "prefix"),
            choice("", "postfix")
        ).
        simple("jsonEscapingCombo", [] (
                Check& check,
                char c, const string& prefix, const string& postfix
            )
        {
            string input(prefix);
            input += c;
            input += postfix;

            string result(prefix);
            result += '\\';
            result += c;
            result += postfix;

            check(M_VAR(jsonEscape(input)) == result);
        }
    )
);
```

Some things to note:

- `choice` simply returns a `vector<decltype(firstArgument)>`. We could have used anything that works in a range based
for loop. Perhaps a `boost::irange` or a container that generates `N` random numbers, or even a constant.
- `static Test::Unit u(...)` creates a context to which we can add many unit tests or sub-contexts.
- `context("Util", ...)` creates a nested context. There's no limit on the depth of nesting.
- `M_VAR` is the only macro used here. All it does is add file and line information to a C++ function call.

That's cool, but I really just want a quick and simple test.

No problem:

```C++
simple("jsonEscape", [] (Check& check) {
    check(M_VAR(jsonEscape("")) == "");
    check(M_VAR(jsonEscape(helloWorld)) == helloWorld);
    check(M_VAR(jsonEscape("prefix\u0001postfix")) == "prefix\\u0001postfix");
    check(M_VAR(jsonEscape("prefix\u0010postfix")) == "prefix\\u0010postfix");
})
```

What about BDD?

```C++
    class BDDTest final: public Test::GWT {
    public:
        BDDTest() : Test::GWT(
            Given("some starting point"),
            When("we do something"),
            Then("we can verify the result")
        )
        // Set up the test.
        {}

        void when() {
            // Do something
        }

        void then() {
            // Check the result
        }
    };

    static Test::Unit u(
        context("SomeTest",
            gwt<BDDTest>("BDDTest")
        )
    );
```

And if I want to parameterize my test?

Easy.

```C++
    void someSimpleFunction(Check& check, int number, int anotherNumber);
    
    static Test::Unit u(
        context("SomeTest",
            gwt<BDDTest>("BDDTest", someContructorArgument),
            simple("simpleTest", someSimpleFunction, 1, 2)
        )
    );
```

## Further Examples

Take a look at the tests for [Util](test/src/TestUtil.cpp) or [Container](test/src/TestContainer.cpp) 

## TODO

I like the sound of this so far. What output formats do you support?

Ahem, you know when I said this project was in it's infancy? Well that's one of the things that currently sucks. It
just logs the output in json format which is not very readable.

Some other stuff that really needs fixing:

- It's only been built on linux with gcc 4.9.
- Exhaustive testing doesn't tell you which scenarios failed, it just logs the check that failed.
- Documentation.
- It depends on boost. This is the only thing stopping it compile with clang.
- Probably a load more stuff. Please log an issue or add a +1 to an existing issue.

# CppUnitTest

[![Build Status](https://travis-ci.org/simon-bourne/CppUnitTest.png)](https://travis-ci.org/simon-bourne/CppUnitTest)

## Features

- Exhaustively test every combination of parameters for a test. 
- Extensible assertions.
- Minimal use of macros. You'll only need `M_EXPR`, and that's not even doing anything clever.
- Nested contexts.
- Simple or BDD style tests.
- Tests will run through all checks and report all errors so you see everything that failed in a test, not just the
first thing.

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

            check(M_EXPR(jsonEscape(input)) == result);
        }
    )
);
```

Some things to note:

- `choice` simply returns a `vector<decltype(firstArgument)>`. We could have used anything that works in a range based
`for` loop. Perhaps a `boost::irange` or a container that generates `N` random numbers, or even a constant.
- `static Test::Unit u(...)` creates a context to which we can add many unit tests or sub-contexts.
- `context("Util", ...)` creates a nested context. There's no limit on the depth of nesting.
- `M_EXPR` is the only macro used here. All it does is add file and line information to a C++ function call.

Can I customize assertions?

Yes. There's not really anything you need to do. For example, if you wrote this function:

```C++
int sum3(int x, int y, int z) {
    return x + y + z;
}
```

You can automatically use it in assertions:

```C++
int a = 1;
int b = 2;
int c = 3;
check(M_EXPR(sum3)(a, b, c) == 6); // This will pass.
check(M_EXPR(sum3)(a, b, c) == 7); // This will fail with the message:
    // Test failed: (sum3(1, 2, 3) == 7)
    //    sum3 == function: in file /path/to/file.cpp, line 5
```

You can also have more complex expressions and add context expressions to give more detail on the output:

```C++
int a = 1;
int b = 2;
int c = 3;
int contextVariable1 = 10;
const char* contextVariable2 = "Looks like something went wrong!";
check(M_EXPR(sum3)(a, b, c) == 7 && a == b, M_EXPR(contextVariable1), M_EXPR(contextVariable2));
```

will fail with the message:
```
Test failed: ((sum3(1, 2, 3) == 7) && (a == b))
    sum3 == function: in file /path/to/file.cpp, line 195
    a == 1: in file /path/to/file.cpp, line 195
    b == 2: in file /path/to/file.cpp, line 195
    context1 == 10: in file /path/to/file.cpp, line 195
    context2 == Looks like something went wrong!: in file /path/to/file.cpp, line 195
```

That's all very cool, but I really just want a quick and simple test.

No problem:

```C++
simple("jsonEscape", [] (Check& check) {
    check(M_EXPR(jsonEscape("")) == "");
    check(M_EXPR(jsonEscape(helloWorld)) == helloWorld);
    check(M_EXPR(jsonEscape("prefix\u0001postfix")) == "prefix\\u0001postfix");
    check(M_EXPR(jsonEscape("prefix\u0010postfix")) == "prefix\\u0010postfix");
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

Ahem, that's one of the things that currently sucks. It just logs the output in json format which is not very readable.

Some other stuff that really needs fixing:

- It's only been built on linux with gcc 4.9.
- Exhaustive testing doesn't tell you which scenarios failed, it just logs the check that failed.
- Documentation.
- It depends on boost. This is the only thing stopping it compile with clang.
- Probably a load more stuff. Please log an issue or add a +1 to an existing issue.

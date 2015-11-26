// Copyright (C) 2014 Simon Bourne

#pragma once

#include "Enhedron/Util.h"
#include "Enhedron/Util/MetaProgramming.h"
#include "Enhedron/Util/Enum.h"
#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Assertion.h"
#include "Enhedron/Test/Results.h"
#include "Enhedron/Container/StringTree.h"

#include "Enhedron/Util/Optional.h"

#include <functional>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <stdexcept>
#include <tuple>
#include <iostream>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Suite {
    using namespace Assertion;

    using Container::StringTree;

    using Util::TaggedValue;
    using Util::StoreArgs;
    using Util::DecayArrayAndFunction_t;
    using Util::bindFirst;

    using Util::optional;

    using std::forward;
    using std::vector;
    using std::move;
    using std::exception;
    using std::function;
    using std::string;
    using std::unique_ptr;
    using std::make_unique;
    using std::tuple;
    using std::index_sequence;
    using std::index_sequence_for;
    using std::get;
    using std::remove_reference;
    using std::bind;
    using std::ref;
    using std::runtime_error;
    using std::cout;

    class Context: public NoCopy {
    public:
        virtual ~Context() {}

        virtual void list(const StringTree& pathTree, Out<ResultContext> results) const = 0;
        virtual Stats run(const StringTree& pathTree, Out<ResultContext> results) = 0;
    };

    using ContextList = vector<unique_ptr<Context>>;

    class Register final: public NoCopyMove {
    public:
        static void add(unique_ptr<Context> context) {
            instance().contextList.emplace_back(move(context));
        }

        static void list(const StringTree& pathTree, Out<Results> results) {
            for (const auto& context : instance().contextList) {
                context->list(pathTree, results);
            }
        }

        static Stats run(const StringTree& pathTree, Out<Results> results) {
            Stats stats;

            for (const auto& context : instance().contextList) {
                stats += context->run(pathTree, results);
            }

            results->finish(stats);
            return stats;
        }
    private:
        Register() = default;

        static Register& instance() {
            static Register theInstance;

            return theInstance;
        }

        ContextList contextList;
    };

    class NodeContext final: public Context {
    public:
        NodeContext(const string& name, vector<unique_ptr<Context>> contextList) :
            name(name),
            contextList(move(contextList))
        {}

        virtual void list(const StringTree& pathTree, Out<ResultContext> results) const override {
            auto child = getChildTree(pathTree);

            if (child) {
                auto resultChild = results->child(name);

                for (const auto& context : contextList) {
                    context->list(*child, out(*resultChild));
                }
            }
        }

        virtual Stats run(const StringTree& pathTree, Out<ResultContext> results) override {
            auto child = getChildTree(pathTree);
            Stats stats;

            if (child) {
                auto resultChild = results->child(name);

                for (const auto& context : contextList) {
                    stats += context->run(*child, out(*resultChild));
                }
            }

            return stats;
        }
    private:
        optional<const StringTree&> getChildTree(const StringTree& pathTree) const {
            if (pathTree.empty()) {
                return pathTree;
            }

            return pathTree.get(name);
        }

        string name;
        vector<unique_ptr<Context>> contextList;
    };

    template<typename... ContextListType>
    void makeContextList(Out<ContextList> contextList, unique_ptr<Context> childContext, ContextListType&&... childContextList) {
        contextList->emplace_back(move(childContext));
        makeContextList(contextList, forward<ContextListType>(childContextList)...);
    }

    inline void makeContextList(Out<ContextList>) {
    }

    template<typename... ContextListType>
    unique_ptr<Context> context(const string& name, ContextListType&&... childContextList) {
        ContextList contextList;
        makeContextList(out(contextList), forward<ContextListType>(childContextList)...);

        return make_unique<NodeContext>(name, move(contextList));
    }

    class Suite: public NoCopy {
    public:
        template<typename... ContextListType>
        Suite(const string& name, ContextListType&&... childContextList) {
            Register::add(context(name, forward<ContextListType>(childContextList)...));
        }
    };

    class WhenRunner {
        struct StackElement {
            size_t index = 0;
            size_t current = 0;
        };

        vector<StackElement> whenStack;
        vector<unique_ptr<ResultTest>> whenCurrentStack;
        size_t whenDepth_ = 0;

        Out<ResultTest> results_;
        Stats stats_;

        bool topWhenDone() const {
            const auto& top = whenStack.back();

            return top.current == top.index + 1;
        }
    public:
        WhenRunner(Out<ResultTest> results) : results_(results) {}

        template<typename Functor, typename... Args>
        void run(Functor&& functor, Args&&... args);

        template<typename Functor>
        void when(string description, Functor&& functor) {
            if (whenStack.size() <= whenDepth_) {
                whenStack.push_back(StackElement{});
            }

            Finally stack([&] { ++whenStack[whenDepth_].current; });

            if (whenStack[whenDepth_].index == whenStack[whenDepth_].current) {
                ++whenDepth_;
                whenCurrentStack.push_back(topResult()->section("when", move(description)));

                Finally depth([&] {
                    --whenDepth_;
                    whenCurrentStack.pop_back();
                });

                stats_.addTest();

                functor();
            }
        }

        Out<ResultTest> topResult() {
            if (whenCurrentStack.empty()) {
                return results_;
            }

            return out(*whenCurrentStack.back());
        }

        Stats stats() const { return stats_; }
    };

    class Check : public NoCopy {
        Out<WhenRunner> whenRunner_;
        Stats stats_;

        friend inline Stats checkStats(const Check& check) {
            return check.stats_;
        }
    public:
        Check(Out<WhenRunner> whenRunner) : whenRunner_(whenRunner) {}

        template<typename Functor>
        void when(string description, Functor&& functor) {
            whenRunner_->when(move(description), forward<Functor>(functor));
        }

        template<
                typename Expression,
                typename... ContextVariableList,
                IsExpression<Expression> = nullptr
        >
        bool operator()(
                string description,
                Expression expression,
                ContextVariableList... contextVariableList
        )
        {
            stats_.addCheck();

            auto ok = CheckWithFailureHandler(
                    whenRunner_->topResult(),
                    move(expression),
                    move(contextVariableList)...
            );

            if ( ! ok) {
                stats_.failCheck();
            }

            return ok;
        }

        template<
                typename Expression,
                typename... ContextVariableList,
                IsExpression<Expression> = nullptr
        >
        bool operator()(
                Expression expression,
                ContextVariableList... contextVariableList
        )
        {
            auto description = expression.makeName();
            return operator()(move(description), move(expression), move(contextVariableList)...);
        }

        template<
                typename Exception = exception,
                typename Expression,
                typename... ContextVariableList,
                IsExpression<Expression> = nullptr
        >
        bool throws(
                Expression expression,
                ContextVariableList... contextVariableList
        )
        {
            return throws(
                    expression.makeName(),
                    move(expression),
                    move<ContextVariableList>(contextVariableList)...
                );
        }

        template<typename Exception = exception, typename Expression, typename... ContextVariableList>
        bool throws(
                string description,
                Expression expression,
                ContextVariableList... contextVariableList
        )
        {
            auto ok = CheckThrowsWithFailureHandler<Exception>(
                    whenRunner_->topResult(),
                    move(expression),
                    move(contextVariableList)...
            );

            stats_.addCheck();

            if ( ! ok) {
                stats_.failCheck();
            }

            return ok;
        }

        template<typename Expression, typename... ContextVariableList>
        void fail(Expression expression, ContextVariableList... contextVariableList) {
            processFailure(whenRunner_->topResult(), move(expression), move(contextVariableList)...);
            stats_.failCheck();
        }
    };

    template<typename Functor, typename... Args>
    void WhenRunner::run(Functor&& functor, Args&&... args) {
        whenStack.clear();
        whenDepth_ = 0;
        stats_.addFixture();

        do {
            for (auto& element : whenStack) {
                element.current = 0;
            }

            Check check(out(*this));

            try {
                functor(check, forward<Args>(args)...);
            }
            catch (const exception& e) {
                results_->failByException(e);
                stats_.failTest();
            }

            stats_ += checkStats(check);

            while ( ! whenStack.empty() && topWhenDone()) {
                whenStack.pop_back();
            }

            if ( ! whenStack.empty()) {
                ++whenStack.back().index;
            }
        } while ( ! whenStack.empty());

        if (stats_.tests() == 0) {
            stats_.addTest();
        }
    }


    template<typename Functor, typename... Args>
    class Runner final: public Context {
        bool included(const StringTree& pathTree) const {
            return pathTree.empty() || bool(pathTree.get(name));
        }

        string name;
        Functor runTest;
        StoreArgs<Args...> args;
    public:
        Runner(string name, Functor runTest, Args&&... args) :
            name(move(name)), runTest(move(runTest)), args(forward<Args>(args)...)
        {}

        virtual void list(const StringTree& pathTree, Out<ResultContext> results) const override {
            if ( ! included(pathTree)) return;

            results->listTest(name);
        }

        // Must only be called once as it forwards the constructor arguments to the class.
        virtual Stats run(const StringTree& pathTree, Out<ResultContext> results) override {
            if ( ! included(pathTree)) return Stats{};

            return args.applyExtraBefore(runTest, name, results);
        }
    };

    template<typename Functor, typename... Args>
    class RunTest final: public NoCopy {
        Functor runTest;
    public:
        RunTest(Functor runTest) : runTest(move(runTest)) {}

        // Visual C++ 2015 seems to generate a dodgy move constructor here.
        RunTest(RunTest&& other) : runTest(move(other.runTest)) {}

        RunTest operator=(RunTest&& other) {
            runTest = move(other.runTest);
        }

        Stats operator()(const string& name, Out<ResultContext> results, Args&&... args) {
            auto test = results->test(name);
            WhenRunner whenRunner(out(*test));
            whenRunner.run(runTest, forward<Args>(args)...);

            return whenRunner.stats();
        }
    };

    // Workaround for MSVC bug. We should be able to use std forward, but it doesn't work for decayed arrays.
    template<typename T>
    struct Forward {
        static constexpr T&& run( typename std::remove_reference<T>::type& t ) {
            return std::forward<T>(t);
        }

        static constexpr T&& run( typename std::remove_reference<T>::type&& t ) {
            return std::forward<T>(t);
        }
    };

    template<typename T>
    struct Forward<T*> {
        static T* run(T* t ) {
            return t;
        }
    };

    template<typename Functor, typename... Args>
    unique_ptr<Context> given(string name, Functor runTest, Args&&... args) {
        return make_unique<Runner<RunTest<Functor, DecayArrayAndFunction_t<Args>...>, DecayArrayAndFunction_t<Args>...>>(
                move(name),
                RunTest<Functor, DecayArrayAndFunction_t<Args>...>(runTest),
                Forward<DecayArrayAndFunction_t<Args>>::run(args)...
            );
    }

    template<typename Functor, typename... Args>
    class RunExhaustive final: public NoCopy {
        Functor runTest;
        StoreArgs<Args...> args;

        template<typename BoundFunctor>
        static Stats exhaustive(BoundFunctor&& functor, const string& name, Out<ResultTest> test) {
            WhenRunner whenRunner(out(*test));
            whenRunner.run(forward<BoundFunctor>(functor));

            return whenRunner.stats();
        }

        template<typename BoundFunctor, typename Container, typename... BoundArgs>
        static Stats exhaustive(
                BoundFunctor&& functor,
                const string& name,
                Out<ResultTest> test,
                const Container& container,
                const BoundArgs&... tail
            )
        {
            Stats stats;

            for (const auto& value : container) {
                stats += exhaustive(
                        [&] (Check& check, auto&&... args) {
                            functor(ref(check), value, args...);
                        },
                        name,
                        test,
                        tail...
                    );
            }

            return stats;
        }
    public:
        RunExhaustive(Functor runTest, StoreArgs<Args...> args) : runTest(move(runTest)), args(move(args)) {}

        Stats operator()(const string& name, Out<ResultContext> results) {
            auto test = results->test(name);

            return args.apply([&] (const Args&... extractedArgs) {
                return exhaustive(move(runTest), name, out(*test), extractedArgs...);
            });
        }
    };

    template<typename Value, typename... Args>
    vector<Value> choice(Value value, Args&&... tail) {
        return vector<Value>{value, forward<Args>(tail)...};
    }

    template<typename Value>
    vector<Value> constant(Value value) {
        return vector<Value>(1, value);
    }

    template<typename... Args>
    class Exhaustive final: public NoCopy {
        StoreArgs<Args...> args;
    public:
        Exhaustive(Args&&... args) : args(forward<Args>(args)...) {}

        template<typename Functor>
        unique_ptr<Context> given(string name, Functor runTest) {
            return make_unique<Runner<RunExhaustive<Functor, Args...>>>(
                    move(name),
                    RunExhaustive<Functor, Args...>(move(runTest), move(args))
            );
        }
    };

    template<typename... Args>
    Exhaustive<Args...> exhaustive(Args&&... args) {
        return Exhaustive<DecayArrayAndFunction_t< Args>...>(forward<DecayArrayAndFunction_t< Args>>(args)...);
    }

    inline void list(const StringTree& pathTree, Out<Results> results) {
        Register::list(pathTree, results);
    }

    inline bool run(const StringTree& pathTree, Out<Results> results) {
        auto stats = Register::run(pathTree, results);

        return stats.failedTests() == 0 && stats.failedChecks() == 0;
    }

    inline void list(const StringTree& pathTree, Verbosity verbosity) {
        HumanResults results(out(cout), verbosity);
        list(pathTree, out(results));
    }

    inline bool run(const StringTree& pathTree, Verbosity verbosity) {
        HumanResults results(out(cout), verbosity);
        return run(pathTree, out(results));
    }
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Suite::Suite;
    using Impl::Impl_Suite::context;
    using Impl::Impl_Suite::Check;
    using Impl::Impl_Suite::given;
    using Impl::Impl_Suite::Exhaustive;
    using Impl::Impl_Suite::exhaustive;
    using Impl::Impl_Suite::choice;
    using Impl::Impl_Suite::constant;
    using Impl::Impl_Suite::list;
    using Impl::Impl_Suite::run;
}}

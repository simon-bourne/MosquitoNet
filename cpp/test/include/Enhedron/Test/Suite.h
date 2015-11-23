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

    class Stats {
    public:
        Stats() = default;

        static Stats ok() { return Stats(1, 0); }
        static Stats fail() { return Stats(1, 1); }

        Stats& operator+=(Stats rhs) {
            ran_ += rhs.ran_;
            failed_ += rhs.failed_;
            return *this;
        }

        auto ran() const { return ran_; }
        auto failed() const { return failed_; }
    private:
        Stats(uint64_t ran, uint64_t failed) : ran_(ran), failed_(failed) {}

        uint64_t ran_ = 0;
        uint64_t failed_ = 0;
    };

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

            results->finish(stats.ran(), stats.failed());
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

    struct NullAction final: public NoCopy {
        static constexpr const char* name = "Test";
        static void run() {}
    };

    class Check : public NoCopy {
        using FailureHandler = CoutFailureHandler<NullAction>;

        struct Status {
            bool failed;
            string description;
        };

        vector<Status> statusList;

        friend Stats logFailures(const Check& checker, const string& name, Out<ResultTest> results);

        bool logChecks(const string& name, Out<ResultTest> results) const {
            bool failed = false;

            for (const auto& status : statusList) {
                if (status.failed) {
                    failed = true;
                    results->failCheck(status.description);
                }
                else {
                    results->passCheck(status.description);
                }
            }

            return failed;
        }
    public:
        Check() = default;

        void addException(const exception& e) {
            statusList.emplace_back(Status{ true, e.what()} );
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
            auto ok = CheckWithFailureHandler<FailureHandler>(
                    move(expression),
                    move(contextVariableList)...
            );
            statusList.push_back(Status { ! ok, move(description) });

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
            auto ok = CheckThrowsWithFailureHandler<FailureHandler, Exception>(
                    move(expression),
                    move(contextVariableList)...
            );
            statusList.push_back(Status { ! ok, move(description) });

            return ok;
        }

        template<typename Expression, typename... ContextVariableList>
        void fail(Expression expression, ContextVariableList... contextVariableList) {
            statusList.push_back(Status {true, expression.evaluate()});
            ProcessFailure<FailureHandler>(move(expression), move(contextVariableList)...);
        }
    };

    inline Stats logFailures(const Check& checker, const string& name, Out<ResultTest> results) {
        if (checker.logChecks(name, results)) {
            return Stats::fail();
        }
        
        return Stats::ok();
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
            Check check;

            try {
                runTest(check, forward<Args>(args)...);
            }
            catch (const exception& e) {
                check.addException(e);
            }

            return logFailures(check, name, out(*test));
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
        static void exhaustiveImpl(BoundFunctor&& functor) {
            functor();
        }

        template<typename BoundFunctor, typename Container, typename... BoundArgs>
        static void exhaustiveImpl(
                BoundFunctor&& functor,
                Container&& container,
                BoundArgs&&... tail
            )
        {
            for (auto&& value : container) {
                exhaustiveImpl(
                        bindFirst(forward<BoundFunctor>(functor), value, index_sequence_for<BoundArgs...>()),
                        forward<BoundArgs>(tail)...
                    );
            }
        }

        static void exhaustive(
                Functor&& functor,
                Check& check,
                Args&&... extractedArgs
        )
        {
            exhaustiveImpl(
                    bindFirst(forward<Functor>(functor), ref(check), index_sequence_for<Args...>()),
                    forward<Args>(extractedArgs)...
                );
        }
    public:
        RunExhaustive(Functor runTest, StoreArgs<Args...> args) : runTest(move(runTest)), args(move(args)) {}

        Stats operator()(const string& name, Out<ResultContext> results) {
            auto test = results->test(name);
            Check check;

            try {
                args.applyExtraBefore(exhaustive, move(runTest), ref(check));
            }
            catch (const exception& e) {
                check.addException(e);
            }

            return logFailures(check, name, out(*test));
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
        return Register::run(pathTree, results).failed() == 0;
    }

    inline void list(const StringTree& pathTree) {
        HumanResults results(out(cout));
        list(pathTree, out(results));
    }

    inline bool run(const StringTree& pathTree) {
        HumanResults results(out(cout));
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

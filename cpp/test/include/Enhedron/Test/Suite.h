// Copyright (C) 2014 Simon Bourne

#pragma once

#include "Enhedron/Util.h"
#include "Enhedron/Util/MetaProgramming.h"
#include "Enhedron/Log.h"
#include "Enhedron/Util/Enum.h"
#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Assertion.h"
#include "Enhedron/Test/Module.h"
#include "Enhedron/Test/Results.h"
#include "Enhedron/Container/StringTree.h"

#include <boost/optional/optional.hpp>

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
    using Util::DecayArray_t;
    using Util::bindFirst;

    using boost::optional;

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
        using FailureHandler = LogFailureHandler<NullAction>;

        struct Status {
            bool failed;
            string description;
        };

        vector<Status> statusList;

        friend void throwOnFailure(const Check& checker, const string& name, Out<ResultTest> results);

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
        Check(
                string description,
                Expression expression,
                ContextVariableList... contextVariableList
        )
        {
            operator()(move(description), move<Expression>(expression), move<ContextVariableList>(contextVariableList)...);
        }

        template<
                typename Expression,
                typename... ContextVariableList,
                IsExpression<Expression> = nullptr
        >
        Check(
                Expression expression,
                ContextVariableList... contextVariableList
        )
        {
            operator()(move<Expression>(expression), move<ContextVariableList>(contextVariableList)...);
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
            auto ok = CheckWithFailureHandler<LogFailureHandler<NullAction>>(
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

    inline void throwOnFailure(const Check& checker, const string& name, Out<ResultTest> results) {
        log.raiseIf(checker.logChecks(name, results), "checkFailed");
    }

    enum class Tag {
        Given,
        When,
        Then
    };

    using Given = TaggedValue<Tag, Tag::Given, string>;
    using When = TaggedValue<Tag, Tag::When, string>;
    using Then = TaggedValue<Tag, Tag::Then, string>;

    template<typename TestClass, typename... Args>
    void runGwt(const string& name, Out<ResultContext> results, Args&&... args);

    class GWT: public NoCopy {
        Given givenText;
        When whenText;
        Then thenText;
        Check checker;

        template<typename TestClass, typename... Args>
        friend void runGwt(const string& name, Out<ResultContext> results, Args&&... args);
    public:
        GWT(Given given, When when, Then then) :
            givenText(move(given)), whenText(move(when)), thenText(move(then)) {}

        void addException(const exception& e) {
            checker.addException(e);
        }

        template<typename... Args>
        auto check(Args&&... args) { return checker(forward<Args>(args)...); }

        template<typename... Args>
        auto checkThrows(Args&&... args) { return checker.throws(forward<Args>(args)...); }

        template<typename... Args>
        auto fail(Args&&... args) { return checker.fail(forward<Args>(args)...); }
    };

    template<typename TestClass, typename... Args>
    void runGwt(const string& name, Out<ResultContext> results, Args&&... args) {
        auto gwtResults(results->gwtTest(name));
        auto given(gwtResults->init());
        TestClass test{forward<Args>(args)...};
        given.close();

        gwtResults->given(*test.givenText);

        try {
            {
                auto whenBlock(gwtResults->when(*test.whenText));
                test.when();
            }

            {
                auto thenBlock(gwtResults->then(*test.thenText));
                test.then();
            }
        }
        catch (const exception& e) {
            test.checker.addException(e);
        }

        throwOnFailure(test.checker, name, out(*gwtResults));
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

            try {
                args.applyExtraBefore(runTest, name, results);

                return Stats::ok();
            }
            catch (const exception& e) {
            }

            return Stats::fail();
        }
    };

    template<typename TestClass, typename... Args>
    unique_ptr<Context> gwt(string name, Args&&... args) {
        return make_unique<
                    Runner<function<void(
                            DecayArray_t< Args>&&...,
                            const string&, Out<ResultContext>
                    )>,
                    DecayArray_t< Args>...>
                >
            (
                move(name),
                runGwt<TestClass, Args...>,
                forward<Args>(args)...
            );
    }

    template<typename Functor, typename... Args>
    class RunSimple final: public NoCopy {
        Functor runTest;
    public:
        RunSimple(Functor runTest) : runTest(move(runTest)) {}

        void operator()(const string& name, Out<ResultContext> results, Args&&... args) {
            auto test = results->simpleTest(name);
            Check check;

            try {
                runTest(check, forward<Args>(args)...);
            }
            catch (const exception& e) {
                check.addException(e);
            }

            throwOnFailure(check, name, out(*test));
        }
    };

    template<typename Functor, typename... Args>
    unique_ptr<Context> simple(string name, Functor runTest, Args&&... args) {
        return make_unique<Runner<RunSimple<Functor, DecayArray_t< Args>...>, DecayArray_t< Args>...>>(
                move(name),
                RunSimple<Functor, DecayArray_t< Args>...>(runTest),
                forward<DecayArray_t< Args>>(args)...
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

        void operator()(const string& name, Out<ResultContext> results) {
            auto test = results->simpleTest(name);
            Check check;

            try {
                args.applyExtraBefore(exhaustive, move(runTest), ref(check));
            }
            catch (const exception& e) {
                check.addException(e);
            }

            throwOnFailure(check, name, out(*test));
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
        unique_ptr<Context> simple(string name, Functor runTest) {
            return make_unique<Runner<RunExhaustive<Functor, Args...>>>(
                    move(name),
                    RunExhaustive<Functor, Args...>(move(runTest), move(args))
            );
        }
    };

    template<typename... Args>
    Exhaustive<Args...> exhaustive(Args&&... args) {
        return Exhaustive<DecayArray_t< Args>...>(forward<DecayArray_t< Args>>(args)...);
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
    using Impl::Impl_Suite::simple;
    using Impl::Impl_Suite::GWT;
    using Impl::Impl_Suite::gwt;
    using Impl::Impl_Suite::Given;
    using Impl::Impl_Suite::When;
    using Impl::Impl_Suite::Then;
    using Impl::Impl_Suite::Exhaustive;
    using Impl::Impl_Suite::exhaustive;
    using Impl::Impl_Suite::choice;
    using Impl::Impl_Suite::constant;
    using Impl::Impl_Suite::list;
    using Impl::Impl_Suite::run;
}}

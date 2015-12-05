//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "Enhedron/Util.h"
#include "Enhedron/Util/MetaProgramming.h"
#include "Enhedron/Util/Enum.h"
#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Assertion.h"
#include "Enhedron/Test/Results.h"

#include "Enhedron/Util/Optional.h"

#include <functional>
#include <algorithm>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <stdexcept>
#include <tuple>
#include <iostream>
#include <regex>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Suite {
    using namespace Assertion;

    using Util::TaggedValue;
    using Util::StoreArgs;
    using Util::DecayArrayAndFunction_t;
    using Util::bindFirst;

    using Util::optional;
    using Util::none;

    using std::forward;
    using std::vector;
    using std::move;
    using std::exception;
    using std::function;
    using std::string;
    using std::unique_ptr;
    using std::shared_ptr;
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
    using std::regex;
    using std::regex_match;
    using std::reference_wrapper;
    using std::min;

    using PathList = vector<shared_ptr<vector<regex>>>;

    class ContextResultsRecorder final : public NoCopy {
        Out<Results> results_;
        NameStack contextStack_;
    public:
        ContextResultsRecorder(Out<Results> results) : results_(results) {
        }

        const NameStack& contextStack() const { return contextStack_; }

        void push(string name) {
            results_->beginContext(contextStack_, name);
            contextStack_.push(move(name));
        }

        void pop(const Stats& stats) {
            auto name = contextStack_.stack().back();
            results_->endContext(stats, contextStack_, name);
            contextStack_.pop();
        }

        Out<Results> beginGiven(const string& name) { results_->beginGiven(contextStack_, name); return results_; }

        void endGiven(const Stats& stats, const string& name) { results_->endGiven(stats, contextStack_, name); }
    };

    class WhenResultRecorder final: public FailureHandler {
        Out<Results> results_;
        const NameStack& contextStack_;
        string given_;
        NameStack whenStack_;
    public:
        WhenResultRecorder(Out<Results> results, const NameStack& contextStack, string given) :
                results_(results), contextStack_(contextStack), given_(move(given)) {}

        void push(string name) {
            results_->beginWhen(contextStack_, given_, whenStack_, name);
            whenStack_.push(move(name));
        }

        void pop(const Stats& stats) {
            string name = whenStack_.stack().back();
            whenStack_.pop();
            results_->endWhen(stats, contextStack_, given_, whenStack_, name);
        }

        void failByException(const exception& e) {
            results_->failByException(contextStack_, given_, whenStack_, e);
        }

        virtual bool notifyPassing() const override { return results_->notifyPassing(); }

        virtual void fail(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            return results_->fail(contextStack_, given_, whenStack_, description, expressionText, variableList);
        }

        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            return results_->pass(contextStack_, given_, whenStack_, description, expressionText, variableList);
        }

    };

    class Context: public NoCopy {
    public:
        virtual ~Context() {}

        virtual void list(const PathList& pathList, Out<ContextResultsRecorder> results, size_t depth) const = 0;
        virtual Stats run(const PathList& pathList, Out<ContextResultsRecorder> resultStack, size_t depth) = 0;
    };

    using ContextList = vector<unique_ptr<Context>>;

    class Register final: public NoCopyMove {
    public:
        static void add(unique_ptr<Context> context) {
            instance().contextList.emplace_back(move(context));
        }

        static void list(const PathList& pathList, Out<Results> results) {
            ContextResultsRecorder resultsRecorder(results);

            for (const auto& context : instance().contextList) {
                context->list(pathList, out(resultsRecorder), 0);
            }
        }

        static Stats run(const PathList& pathList, Out<Results> results) {
            Stats stats;

            ContextResultsRecorder resultsRecorder(results);

            for (const auto& context : instance().contextList) {
                stats += context->run(pathList, out(resultsRecorder), 0);
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

        virtual void list(const PathList& pathList, Out<ContextResultsRecorder> results, size_t depth) const override {
            auto nextPathList = getMatchingPaths(pathList, depth);
            results->push(name);

            if (nextPathList) {
                for (const auto& context : contextList) {
                    context->list(*nextPathList, results, depth + 1);
                }
            }

            Stats stats;
            results->pop(stats); // TODO
        }

        virtual Stats run(const PathList& pathList, Out<ContextResultsRecorder> results, size_t depth) override {
            Stats stats;
            auto nextPathList = getMatchingPaths(pathList, depth);
            results->push(name);

            if (nextPathList) {
                for (const auto& context : contextList) {
                    stats += context->run(*nextPathList, results, depth + 1);
                }
            }

            results->pop(stats);

            return stats;
        }
    private:
        optional<PathList> getMatchingPaths(const PathList& pathList, size_t depth) const {
            if (pathList.empty()) {
                return pathList;
            }

            vector<shared_ptr<vector<regex>>> nextPathList;

            for (const auto& path : pathList) {
                if (path->size() > depth) {
                    if (regex_match(name, (*path)[depth])) {
                        nextPathList.push_back(path);
                    }
                }
                else {
                    return pathList;
                }
            }

            if (nextPathList.empty()) {
                return none;
            }

            return move(nextPathList);
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

    class WhenRunner final: public FailureHandler {
        struct StackElement {
            size_t index = 0;
            size_t current = 0;
        };

        Stats stats_;
        vector<StackElement> whenStack;
        WhenResultRecorder whenResultRecorder_;
        size_t whenDepth_ = 0;

        bool topWhenDone() const {
            const auto& top = whenStack.back();

            return top.current == top.index + 1;
        }
    public:
        WhenRunner(Out<Results> results, const NameStack& contextStack, string given) :
                whenResultRecorder_(results, contextStack, move(given)) {}

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
                whenResultRecorder_.push(move(description));

                Finally depth([&] {
                    --whenDepth_;
                    Stats stats; // TODO
                    whenResultRecorder_.pop(stats);
                });

                functor();
            }
        }

        virtual bool notifyPassing() const override { return whenResultRecorder_.notifyPassing(); }

        virtual void fail(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            return whenResultRecorder_.fail(description, expressionText, variableList);
        }

        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            return whenResultRecorder_.pass(description, expressionText, variableList);
        }

        const Stats& stats() const { return stats_; }
    };

    class Check : public NoCopy {
        Out<WhenRunner> whenRunner_;
        Stats stats_;

        friend inline Stats checkStats(const Check& check) {
            return check.stats_;
        }

        bool addCheck(bool ok) {
            stats_.addCheck();

            if ( ! ok) {
                stats_.failCheck();
            }

            return ok;
        }
    public:
        Check(Out<WhenRunner> whenRunner) : whenRunner_(whenRunner) {}

        template<typename Functor>
        void when(string description, Functor&& functor) {
            whenRunner_->when(move(description), forward<Functor>(functor));
        }

        template<typename... Args>
        bool operator()(Args&&... args) {
            // TODO: Wrap with failure handler class that has when stack and context stack.
            return addCheck(CheckWithFailureHandler(
                    whenRunner_,
                    forward<Args>(args)...
            ));
        }

        template<typename Exception = exception, typename... Args>
        bool throws(Args&&... args) {
            return addCheck(CheckThrowsWithFailureHandler<Exception>(
                    whenRunner_,
                    forward<Args>(args)...
            ));
        }

        template<typename Expression, typename... ContextVariableList>
        void fail(Expression expression, ContextVariableList... contextVariableList) {
            processFailure(whenRunner_, none, move(expression), move(contextVariableList)...);
            stats_.failCheck();
        }
    };

    template<typename Functor, typename... Args>
    void WhenRunner::run(Functor&& functor, Args&&... args) {
        whenStack.clear();
        whenDepth_ = 0;
        stats_.addFixture(); // TODO:

        do {
            for (auto& element : whenStack) {
                element.current = 0;
            }

            Check check(out(*this));

            try {
                functor(check, forward<Args>(args)...);
            }
            catch (const exception& e) {
                whenResultRecorder_.failByException(e);
                stats_.failTest();
            }

            stats_.addTest(); // TODO

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
        bool included(const PathList& pathList, size_t depth) const {
            if (pathList.empty()) {
                return true;
            }

            for (const auto& path : pathList) {
                if (path->size() > depth) {
                    if (regex_match(name, (*path)[depth])) {
                        return true;
                    }
                }
                else {
                    return true;
                }
            }

            return false;
        }

        string name;
        Functor runTest;
        StoreArgs<Args...> args;
    public:
        Runner(string name, Functor runTest, Args&&... args) :
            name(move(name)), runTest(move(runTest)), args(forward<Args>(args)...)
        {}

        virtual void list(const PathList& pathList, Out<ContextResultsRecorder> results, size_t depth) const override {
            if ( ! included(pathList, depth)) return;

            results->beginGiven(name);
            Stats stats;
            results->endGiven(stats, name);
        }

        // Must only be called once as it forwards the constructor arguments to the class.
        virtual Stats run(const PathList& pathList, Out<ContextResultsRecorder> results, size_t depth) override {
            Stats stats;

            if (included(pathList, depth)) {
                stats += args.applyExtraBefore(runTest, name, results);
            }

            return stats;
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

        Stats operator()(const string& name, Out<ContextResultsRecorder> results, Args&&... args) {
            auto test = results->beginGiven(name);
            WhenRunner whenRunner(out(*test), results->contextStack(), name);
            whenRunner.run(runTest, forward<Args>(args)...);

            auto stats = whenRunner.stats();

            results->endGiven(stats, name);

            return stats;
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
        static Stats exhaustive(BoundFunctor&& functor,
                                const string& name,
                                Out<ContextResultsRecorder> results,
                                Out<Results> test)
        {
            WhenRunner whenRunner(out(*test), results->contextStack(), name);
            whenRunner.run(forward<BoundFunctor>(functor));

            return whenRunner.stats();
        }

        template<typename BoundFunctor, typename Container, typename... BoundArgs>
        static Stats exhaustive(
                BoundFunctor&& functor,
                const string& name,
                Out<ContextResultsRecorder> results,
                Out<Results> test,
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
                        results,
                        test,
                        tail...
                    );
            }

            return stats;
        }
    public:
        RunExhaustive(Functor runTest, StoreArgs<Args...> args) : runTest(move(runTest)), args(move(args)) {}

        Stats operator()(const string& name, Out<ContextResultsRecorder> results) {
            auto test = results->beginGiven(name);

            Stats stats = args.apply([&] (const Args&... extractedArgs) {
                return exhaustive(move(runTest), name, results, out(*test), extractedArgs...);
            });

            results->endGiven(stats, name);

            return stats;
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

    inline void list(const PathList& pathList, Out<Results> results) {
        Register::list(pathList, results);
    }

    inline bool run(const PathList& pathList, Out<Results> results) {
        auto stats = Register::run(pathList, results);

        return stats.failedTests() == 0 && stats.failedChecks() == 0;
    }

    inline void list(const PathList& pathList, Verbosity verbosity) {
        HumanResults results(out(cout), verbosity);
        return list(pathList, out(results));
    }

    inline bool run(const PathList& pathList, Verbosity verbosity) {
        HumanResults results(out(cout), verbosity);
        return run(pathList, out(results));
    }
}}}}

namespace Enhedron { namespace Test {
    using namespace Assertion;

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

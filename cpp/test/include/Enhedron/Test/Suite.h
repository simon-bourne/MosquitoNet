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
        using Stack = vector<unique_ptr<ResultContext>>;

        Stack contextStack_;
        size_t notifiedEnd = 0;
    public:
        ContextResultsRecorder(unique_ptr<ResultContext> root) {
            contextStack_.emplace_back(move(root));
        }

        void pushContext(const string& name) {
            contextStack_.emplace_back(contextStack_.back()->child(name));
        }

        void popContext(const Stats &stats) {
            contextStack_.back()->finish(stats);
            popContext();
        }

        void popContext() {
            contextStack_.pop_back();
            notifiedEnd = 0;
        }

        unique_ptr<ResultTest> test(const string &name) {
            for (
                    auto context = contextStack_.begin() + static_cast<Stack::difference_type>(notifiedEnd);
                    context != contextStack_.end();
                    ++context)
            {
                (*context)->beforeFirstTestRuns();
            }

            notifiedEnd = contextStack_.size();

            return contextStack_.back()->test(name);
        }
    };

    // TODO: WhenBlockResultRecorder

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

        static void list(const PathList& pathList, Out<ContextResultsRecorder> results) {
            for (const auto& context : instance().contextList) {
                context->list(pathList, results, 0);
            }
        }

        static Stats run(const PathList& pathList, Out<ContextResultsRecorder> results) {
            Stats stats;

            for (const auto& context : instance().contextList) {
                stats += context->run(pathList, results, 0);
            }

            results->popContext(stats);
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
            results->pushContext(name);

            if (nextPathList) {
                for (const auto& context : contextList) {
                    context->list(*nextPathList, results, depth + 1);
                }
            }

            results->popContext();
        }

        virtual Stats run(const PathList& pathList, Out<ContextResultsRecorder> results, size_t depth) override {
            Stats stats;
            auto nextPathList = getMatchingPaths(pathList, depth);
            results->pushContext(name);

            if (nextPathList) {
                for (const auto& context : contextList) {
                    stats += context->run(*nextPathList, results, depth + 1);
                }
            }

            results->popContext(stats);

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
                whenCurrentStack.push_back(topResult()->section(move(description)));

                Finally depth([&] {
                    --whenDepth_;
                    whenCurrentStack.pop_back();
                });

                functor();
            }
        }

        // TODO: Return entire stack.
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
                    whenRunner_->topResult(),
                    forward<Args>(args)...
            ));
        }

        template<typename Exception = exception, typename... Args>
        bool throws(Args&&... args) {
            return addCheck(CheckThrowsWithFailureHandler<Exception>(
                    whenRunner_->topResult(),
                    forward<Args>(args)...
            ));
        }

        template<typename Expression, typename... ContextVariableList>
        void fail(Expression expression, ContextVariableList... contextVariableList) {
            processFailure(whenRunner_->topResult(), none, move(expression), move(contextVariableList)...);
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

            // TODO: Pass in context and when stack to this, so check can notify of first failure
            Check check(out(*this));

            try {
                functor(check, forward<Args>(args)...);
            }
            catch (const exception& e) {
                // TODO: if ( ! check.hasFailed()) results_->beforeFirstFailure();
                results_->failByException(e);
                stats_.failTest();
            }

            auto currentStats = checkStats(check);
            currentStats.addTest();
            results_->finish(currentStats);
            stats_ += currentStats;

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

            results->test(name);
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
            auto test = results->test(name);
            WhenRunner whenRunner(out(*test));
            whenRunner.run(runTest, forward<Args>(args)...);

            auto stats = whenRunner.stats();

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

        Stats operator()(const string& name, Out<ContextResultsRecorder> results) {
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

    inline void list(const PathList& pathList, unique_ptr<ResultContext> results) {
        ContextResultsRecorder resultStack(move(results));
        Register::list(pathList, out(resultStack));
    }

    inline bool run(const PathList& pathList, unique_ptr<ResultContext> results) {
        ContextResultsRecorder resultStack(move(results));
        auto stats = Register::run(pathList, out(resultStack));

        return stats.failedTests() == 0 && stats.failedChecks() == 0;
    }

    inline void list(const PathList& pathList, Verbosity verbosity) {
        return list(pathList, make_unique<HumanResultRootContext>(out(cout), verbosity));
    }

    inline bool run(const PathList& pathList, Verbosity verbosity) {
        return run(pathList, make_unique<HumanResultRootContext>(out(cout), verbosity));
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

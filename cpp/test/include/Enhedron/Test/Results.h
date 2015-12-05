//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "Enhedron/Util.h"
#include "Enhedron/Util/Optional.h"

#include <memory>
#include <string>
#include <stdexcept>
#include <vector>
#include <algorithm>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Results {
    using std::exception;
    using std::string;
    using std::unique_ptr;
    using std::make_unique;
    using std::ostream;
    using std::endl;
    using std::vector;
    using std::move;
    using std::min;
    using std::max;

    using Assertion::FailureHandler;
    using Assertion::Variable;
    using Util::optional;

    class Stats {
        uint64_t fixtures_ = 0;
        uint64_t tests_ = 0;
        uint64_t checks_ = 0;
        uint64_t failedTests_ = 0;
        uint64_t failedChecks_ = 0;
    public:
        Stats& operator+=(Stats rhs) {
            fixtures_ += rhs.fixtures_;
            tests_ += rhs.tests_;
            checks_ += rhs.checks_;
            failedTests_ += rhs.failedTests_;
            failedChecks_ += rhs.failedChecks_;
            return *this;
        }

        void addFixture() { ++fixtures_; }
        void addTest() { ++tests_; }
        void addCheck() { ++checks_; }

        void failTest() { ++failedTests_; }
        void failCheck() { ++failedChecks_; }

        uint64_t fixtures() const { return fixtures_; }
        uint64_t tests() const { return tests_; }
        uint64_t checks() const { return checks_; }
        uint64_t failedTests() const { return failedTests_; }
        uint64_t failedChecks() const { return failedChecks_; }
    };

    // Wrapper, as we may add more functionality here. Such as tracking how much of the stack has
    // been seen before.
    class NameStack final: public NoCopy {
        vector<string> stack_;
    public:
        const vector<string>& stack() const { return stack_; }

        void push(string name) {
            stack_.emplace_back(move(name));
        }

        void pop() {
            Assert( ! VAR(stack_.empty()));
            stack_.pop_back();
        }
    };

    struct Results: public NoCopy {
        virtual ~Results() {}

        virtual void finish(const Stats& stats) = 0;

        virtual void beginContext(const NameStack& contextStack, const string& name) = 0;
        virtual void endContext(const Stats& stats, const NameStack& contextStack, const string& name) = 0;

        virtual void beginGiven(const NameStack& context, const string& given) = 0;
        virtual void endGiven(const Stats& stats, const NameStack& context, const string& given) = 0;

        virtual void beginWhen(const NameStack& context,
                               const string& given,
                               const NameStack& whenStack,
                               const string& when) = 0;
        virtual void endWhen(const Stats& stats,
                             const NameStack& context,
                             const string& given,
                             const NameStack& whenStack,
                             const string& when) = 0;

        virtual bool notifyPassing() const = 0;

        virtual void fail(const NameStack& context,
                          const string& given,
                          const NameStack& whenStack,
                          optional<string> description,
                          const string &expressionText,
                          const vector <Variable> &variableList) = 0;

        virtual void pass(const NameStack& context,
                          const string& given,
                          const NameStack& whenStack,
                          optional <string> description,
                          const string &expressionText,
                          const vector <Variable> &variableList) = 0;

        virtual void failByException(const NameStack& context,
                                     const string& given,
                                     const NameStack& whenStack,
                                     const exception& e) = 0;
    };

    enum class Verbosity {
        SILENT,
        SUMMARY,
        CONTEXTS,
        FIXTURES,
        SECTIONS,
        EXHAUSTIVE_SECTIONS,
        CHECKS,
        CHECKS_EXPRESSION,
        VARIABLES
    };

    enum class WrittenState {
        NONE,
        CONTEXT,
        GIVEN
    };

    class HumanResults final: public Results {
        Out<ostream> output_;
        Verbosity verbosity_;
        WrittenState writtenState_ = WrittenState::NONE;
        size_t whenDepth_ = 0;
        size_t whenWrittenDepth_ = 0;

        void setMaxWrittenState(WrittenState maxState) {
            if (writtenState_ > maxState) writtenState_ = maxState;
        }

        bool writeNeeded(WrittenState state) {
            if (writtenState_ < state) {
                writtenState_ = state;
                return true;
            }

            return false;
        }

        void writeContext(const NameStack& contextStack) {
            if (writeNeeded(WrittenState::CONTEXT)) {
                if ( ! contextStack.stack().empty()) {
                    *output_ << contextStack.stack().front();

                    for (
                            auto contextIter = contextStack.stack().begin() + 1;
                            contextIter != contextStack.stack().end();
                            ++contextIter
                        )
                    {
                        *output_ << "/" << *contextIter;
                    }

                    *output_ << "\n";
                }
            }
        }

        void writeGiven(const NameStack& context, const string& given) {
            writeContext(context);

            if (writeNeeded(WrittenState::GIVEN)) {
                indent(1);
                *output_ << "Given: " << given << "\n";
            }
        }

        void writeWhenStack(const NameStack& context, const string& given, const NameStack& when) {
            writeGiven(context, given);

            size_t depth = whenWrittenDepth_;
            auto startIndex = static_cast<vector<string>::difference_type>(whenWrittenDepth_);

            for (
                    auto whenIter = when.stack().begin() + startIndex;
                    whenIter != when.stack().end();
                    ++whenIter
                )
            {
                ++depth;
                indent(depth);
                *output_ << " When: " << *whenIter << "\n";
            }

            whenWrittenDepth_ = when.stack().size();
        }

        void printVariables(const vector <Variable> &variableList) {
            for (const auto& variable : variableList) {
                indent(whenDepth() + 1);
                (*output_) << variable.name() << " = " << variable.value()
                           << ": file \"" << variable.file() << "\", line " << variable.line() << ".\n";
            }
        }

        void indent(size_t indent) {
            while (indent > 0) {
                *output_ << "    ";
                --indent;
            }
        }

        size_t whenDepth() const {
            static constexpr const size_t minDepth = 1;

            return max(minDepth, whenDepth_);
        }
    public:
        HumanResults(Out<ostream> output, Verbosity verbosity) :
            output_(output), verbosity_(verbosity)
        {}

        virtual void finish(const Stats& stats) override {
            if (verbosity_ >= Verbosity::SUMMARY) {
                if (stats.failedTests() > 0) {
                    *output_ << "FAILED TESTS: " << stats.failedTests() << "\n";
                }

                if (stats.failedChecks() > 0) {
                    *output_ << "FAILED CHECKS: " << stats.failedChecks() << "\n";
                }

                *output_ << "Totals: " <<
                stats.tests() << " tests, " <<
                stats.checks() << " checks, " <<
                stats.fixtures() << " fixtures\n";
            }
        }

        virtual void beginContext(const NameStack& contextStack, const string& name) override {
            setMaxWrittenState(WrittenState::NONE);
        }

        virtual void endContext(const Stats& stats, const NameStack& context, const string& name) override {
            setMaxWrittenState(WrittenState::NONE);
        }

        virtual void beginGiven(const NameStack& context, const string& given) override {
            setMaxWrittenState(WrittenState::GIVEN);

            if (verbosity_ >= Verbosity::CONTEXTS) {
                writeContext(context);
            }

            if (verbosity_ >= Verbosity::FIXTURES) {
                writeGiven(context, given);
            }
        }

        virtual void endGiven(const Stats& stats, const NameStack& context, const string& given) override {
            setMaxWrittenState(WrittenState::CONTEXT);
        }

        virtual void beginWhen(const NameStack& context,
                               const string& given,
                               const NameStack& whenStack,
                               const string& when) override {
            ++whenDepth_;

            if (verbosity_ >= Verbosity::SECTIONS) {
                indent(whenDepth());
                *output_ << " When: " << when << "\n";
                whenWrittenDepth_ = whenDepth_;
            }
        }

        virtual void endWhen(const Stats& stats,
                             const NameStack& context,
                             const string& given,
                             const NameStack& whenStack,
                             const string& when) override {
            --whenDepth_;
            whenWrittenDepth_ = min(whenDepth_, whenWrittenDepth_);

            if (whenDepth_ == 0 && verbosity_ >= Verbosity::CHECKS) {
                *output_ << "\n";
            }

            setMaxWrittenState(WrittenState::GIVEN);
        }

        virtual bool notifyPassing() const override { return verbosity_ >= Verbosity::CHECKS; }

        virtual void fail(const NameStack& context,
                          const string& given,
                          const NameStack& whenStack,
                          optional<string> description,
                          const string &expressionText,
                          const vector <Variable> &variableList) override
        {
            writeWhenStack(context, given, whenStack);
            indent(whenDepth());
            (*output_) << "FAILED: Then";

            if (description) {
                *output_ << ": " << *description;
            }

            *output_ << ": " << expressionText << "\n";
            printVariables(variableList);
        }

        virtual void pass(const NameStack& context,
                          const string& given,
                          const NameStack& whenStack,
                          optional <string> description,
                          const string &expressionText,
                          const vector <Variable> &variableList) override
        {
            indent(whenDepth());
            (*output_) << " Then";

            if (description) {
                (*output_) << ": " << *description;
            }

            if (verbosity_ >= Verbosity::CHECKS_EXPRESSION || ! description) {
                (*output_) << ": " << expressionText;
            }

            (*output_) << "\n";

            if (verbosity_ >= Verbosity::VARIABLES) {
                printVariables(variableList);
            }
        }

        virtual void failByException(const NameStack& context,
                                     const string& given,
                                     const NameStack& whenStack,
                                     const exception& e) override {
            indent(whenDepth());
            (*output_) << "TEST FAILED WITH EXCEPTION: " << e.what() << endl;
        }
    };
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Results::NameStack;
    using Impl::Impl_Results::Results;
    using Impl::Impl_Results::HumanResults;
    using Impl::Impl_Results::Stats;
    using Impl::Impl_Results::Verbosity;
}}

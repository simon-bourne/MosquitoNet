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

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Results {
    using std::exception;
    using std::string;
    using std::unique_ptr;
    using std::make_unique;
    using std::ostream;
    using std::endl;
    using std::vector;

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

    class ResultTest: public NoCopy, public FailureHandler {
    public:
        virtual ~ResultTest() {}
        virtual unique_ptr<ResultTest> section(string description) = 0;
        virtual void beforeFirstFailure() = 0;
        virtual void failByException(const exception& e) = 0;
        virtual void finish(const Stats& stats) = 0;
    };

    class ResultContext: public NoCopy {
    public:
        virtual ~ResultContext() {}
        virtual unique_ptr<ResultContext> child(const string& name) = 0;
        virtual void beforeFirstTestRuns() = 0;
        virtual void beforeFirstFailure() = 0;
        virtual unique_ptr<ResultTest> test(const string& name) = 0;
        virtual void finish(const Stats& stats) = 0;
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

    class HumanResultTest final: public ResultTest {
        Out<ostream> outputStream_;
        size_t depth_;
        Verbosity verbosity_;

        void indent(size_t relativeDepth) {
            for (size_t totalDepth = 1 + depth_ + relativeDepth; totalDepth > 0; --totalDepth) {
                (*outputStream_) << "    ";
            }
        }

        void printVariables(const vector <Variable> &variableList) {
            for (const auto& variable : variableList) {
                indent(2);
                (*outputStream_) << variable.name() << " = " << variable.value()
                << ": file \"" << variable.file() << "\", line " << variable.line() << ".\n";
            }
        }
    public:
        HumanResultTest(Out<ostream> outputStream, size_t depth, Verbosity verbosity) :
                outputStream_(outputStream), depth_(depth), verbosity_(verbosity)
        {}

        virtual unique_ptr<ResultTest> section(string description) override {
            if (verbosity_ >= Verbosity::SECTIONS) {
                indent(1);
                (*outputStream_) << "when: " << description << "\n";
            }

            return make_unique<HumanResultTest>(outputStream_, depth_ + 1, verbosity_);
        }

        virtual bool notifyPassing() const override { return verbosity_ >= Verbosity::CHECKS; }

        virtual void beforeFirstFailure() override {}

        virtual void fail(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            indent(1);
            (*outputStream_) << "CHECK FAILED: " << expressionText << "\n";
            printVariables(variableList);
        }

        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            indent(1);
            (*outputStream_) << "then";

            if (description) {
                (*outputStream_) << ": " << *description;
            }

            if (verbosity_ >= Verbosity::CHECKS_EXPRESSION) {
                (*outputStream_) << ": " << expressionText;
            }

            (*outputStream_) << "\n";

            if (verbosity_ >= Verbosity::VARIABLES) {
                printVariables(variableList);
            }
        }

        virtual void failByException(const exception& e) override {
            indent(0);
            (*outputStream_) << "TEST FAILED WITH EXCEPTION: " << e.what() << endl;
        }

        virtual void finish(const Stats& stats) override {}

    };

    class HumanResultContext final: public ResultContext {
        Out <ostream> outputStream_;
        Verbosity verbosity_;
        string path_;

    public:
        HumanResultContext(Out<ostream> outputStream, Verbosity verbosity, const string& name) :
                outputStream_(outputStream), verbosity_(verbosity), path_(name) {}

        virtual void beforeFirstTestRuns() override {
            if (verbosity_ >= Verbosity::CONTEXTS) {
                *outputStream_ << path_ << "\n";
            }
        }

        virtual void beforeFirstFailure() override { }

        virtual unique_ptr<ResultContext> child(const string& name) override {
            string childPath(path_);

            if ( ! path_.empty()) {
                childPath += "/";
            }

            childPath += name;

            return make_unique<HumanResultContext>(outputStream_, verbosity_, childPath);
        }

        virtual unique_ptr<ResultTest> test(const string& name) override {
            if (verbosity_ >= Verbosity::FIXTURES) {
                *outputStream_ << "    Given: " << name << endl;
            }

            return make_unique<HumanResultTest>(outputStream_, 0, verbosity_);
        }

        virtual void finish(const Stats& stats) override {}
    };

    class HumanResultRootContext final: public ResultContext {
        Out <ostream> outputStream_;
        Verbosity verbosity_;
    public:
        HumanResultRootContext(Out<ostream> outputStream, Verbosity verbosity) :
            outputStream_(outputStream), verbosity_(verbosity) {}

        virtual void beforeFirstTestRuns() override { }

        virtual void beforeFirstFailure() override { }

        virtual unique_ptr<ResultContext> child(const string& name) override {
            return make_unique<HumanResultContext>(outputStream_, verbosity_, name);
        }

        virtual unique_ptr<ResultTest> test(const string& name) override {
            return make_unique<HumanResultTest>(outputStream_, 0, verbosity_);
        }

        virtual void finish(const Stats& stats) override {
            if (verbosity_ >= Verbosity::SUMMARY) {
                if (stats.failedTests() > 0) {
                    *outputStream_ << "FAILED TESTS: " << stats.failedTests() << "\n";
                }

                if (stats.failedChecks() > 0) {
                    *outputStream_ << "FAILED CHECKS: " << stats.failedChecks() << "\n";
                }

                *outputStream_ << "Totals: " <<
                stats.tests() << " tests, " <<
                stats.checks() << " checks, " <<
                stats.fixtures() << " fixtures\n";
            }
        }
    };
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Results::ResultContext;
    using Impl::Impl_Results::ResultTest;
    using Impl::Impl_Results::HumanResultRootContext;
    using Impl::Impl_Results::Stats;
    using Impl::Impl_Results::Verbosity;
}}

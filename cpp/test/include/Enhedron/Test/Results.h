// Copyright (C) 2014 Simon Bourne

#pragma once

#include "Enhedron/Util.h"

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
        virtual unique_ptr<ResultTest> section(string name, string description) = 0;
        virtual void pass(const string& description) = 0;
        virtual void failByException(const exception& e) = 0;
        virtual void fail(const string &expressionText, const vector <Variable> &variableList) = 0;
    };

    class ResultContext: public NoCopy {
    public:
        virtual ~ResultContext() {}
        virtual unique_ptr<ResultContext> child(const string& name) = 0;
        virtual void listTest(const string& name) = 0;
        virtual unique_ptr<ResultTest> test(const string& name) = 0;
    };

    class Results: public ResultContext {
    public:
        virtual ~Results() {}
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
    public:
        HumanResultTest(Out<ostream> outputStream, size_t depth, Verbosity verbosity) :
                outputStream_(outputStream), depth_(depth), verbosity_(verbosity)
        {}

        virtual unique_ptr<ResultTest> section(string name, string description) override {
            if (verbosity_ >= Verbosity::SECTIONS) {
                indent(1);
                (*outputStream_) << name << ": " << description << "\n";
            }

            return make_unique<HumanResultTest>(outputStream_, depth_ + 1, verbosity_);
        }

        virtual void fail(const string &expressionText, const vector <Variable> &variableList) override {
            indent(1);
            (*outputStream_) << "Check failed: " << expressionText << "\n";

            for (const auto& variable : variableList) {
                indent(2);
                (*outputStream_) << variable.name() << " = " << variable.value()
                                 << ": file \"" << variable.file() << "\", line " << variable.line() << ".\n";
            }
        }

        virtual void pass(const string& description) override {
            if (verbosity_ >= Verbosity::CHECKS) {
                indent(1);
                (*outputStream_) << "then: " << description << endl;
            }
        }

        virtual void failByException(const exception& e) override {
            indent(0);
            (*outputStream_) << "TEST FAILED! Exception: " << e.what() << endl;
        }
    };

    template<typename Base>
    class HumanResultContext: public Base {
        Out<ostream> outputStream_;
        bool contextPrinted_ = false;
        string path_;
        Verbosity verbosity_;

        void writeTestName(const string& testName) {
            if (verbosity_ >= Verbosity::CONTEXTS) {
                if (!contextPrinted_) {
                    contextPrinted_ = true;
                    *outputStream_ << path_ << "\n";
                }
            }

            if (verbosity_ >= Verbosity::FIXTURES) {
                *outputStream_ << "    " << testName << endl;
            }
        }
    public:
        HumanResultContext(Out<ostream> outputStream, const string& name, Verbosity verbosity) :
            outputStream_(outputStream), path_(name), verbosity_(verbosity) {
        }

        virtual unique_ptr<ResultContext> child(const string& name) override {
            string childPath(path_);
            
            if ( ! path_.empty()) {
                childPath += ".";
            }
            
            childPath += name;
            
            return make_unique<HumanResultContext<ResultContext>>(outputStream_, childPath, verbosity_);
        }

        virtual void listTest(const string& name) override {
            writeTestName(name);
        }

        virtual unique_ptr<ResultTest> test(const string& name) override {
            writeTestName(name);
            return make_unique<HumanResultTest>(outputStream_, 0, verbosity_);
        }
    };

    class HumanResults final: public HumanResultContext<Results> {
        Out<ostream> outputStream_;
        Verbosity verbosity_;
    public:
        HumanResults(Out<ostream> outputStream, Verbosity verbosity) :
                HumanResultContext(outputStream, "", verbosity),
                outputStream_(outputStream),
                verbosity_(verbosity)
        {}

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
    using Impl::Impl_Results::Results;
    using Impl::Impl_Results::ResultContext;
    using Impl::Impl_Results::ResultTest;
    using Impl::Impl_Results::HumanResults;
    using Impl::Impl_Results::Stats;
    using Impl::Impl_Results::Verbosity;
}}

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

    class ResultTest: public NoCopy, public FailureHandler {
    public:
        virtual ~ResultTest() {}
        virtual void passCheck(const string& description) = 0;
        virtual void failCheck(const string& description) = 0;
        virtual void failByException(const exception& e) = 0;
        virtual void handleCheckFailure(const string &expressionText, const vector <Variable> &variableList) = 0;
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
        virtual void finish(uint64_t ran, uint64_t failed) = 0;
    };


    class HumanResultTest final: public ResultTest {
        Out<ostream> outputStream_;

        void indent() {
            (*outputStream_) << "    ";
        }
    public:
        HumanResultTest(Out<ostream> outputStream) :
                outputStream_(outputStream)
        {}

        virtual void handleCheckFailure(const string &expressionText, const vector <Variable> &variableList) override {
            indent();
            indent();
            (*outputStream_) << "Check failed: " << expressionText << "\n";

            for (const auto& variable : variableList) {
                indent();
                indent();
                indent();
                (*outputStream_) << variable.name() << " = " << variable.value()
                                 << ": file \"" << variable.file() << "\", line " << variable.line() << ".\n";
            }
        }

        virtual void passCheck(const string& description) override {}

        virtual void failCheck(const string& description) override {
            indent();
            (*outputStream_) << "TEST FAILED! " << description << endl;
        }

        virtual void failByException(const exception& e) override {
            indent();
            (*outputStream_) << "TEST FAILED! Exception: " << e.what() << endl;
        }
    };

    template<typename Base>
    class HumanResultContext: public Base {
        Out<ostream> outputStream_;
        bool contextPrinted_ = false;
        string path_;

        void writeTestName(const string& testName) {
            if ( ! contextPrinted_) {
                contextPrinted_ = true;
                *outputStream_ << path_ << "\n";
            }

            *outputStream_ << "    " << testName << endl;
        }
    public:
        HumanResultContext(Out<ostream> outputStream, const string& name) :
            outputStream_(outputStream), path_(name) {
        }

        virtual unique_ptr<ResultContext> child(const string& name) override {
            string childPath(path_);
            
            if ( ! path_.empty()) {
                childPath += ".";
            }
            
            childPath += name;
            
            return make_unique<HumanResultContext<ResultContext>>(outputStream_, childPath);
        }

        virtual void listTest(const string& name) override {
            writeTestName(name);
        }

        virtual unique_ptr<ResultTest> test(const string& name) override {
            writeTestName(name);
            return make_unique<HumanResultTest>(outputStream_);
        }
    };

    class HumanResults final: public HumanResultContext<Results> {
        Out<ostream> outputStream_;
    public:
        HumanResults(Out<ostream> outputStream) :
                HumanResultContext(outputStream, ""), outputStream_(outputStream) {}

        virtual void finish(uint64_t ran, uint64_t failed) override {
            uint64_t passed = ran - failed;

            if (failed != 0) {
                *outputStream_ << "FAILED: " << failed << ", ";
            }

            *outputStream_ << "OK: " << passed << endl;
        }
    };
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Results::Results;
    using Impl::Impl_Results::ResultContext;
    using Impl::Impl_Results::ResultTest;
    using Impl::Impl_Results::HumanResults;
}}

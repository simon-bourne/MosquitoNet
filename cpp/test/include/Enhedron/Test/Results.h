// Copyright (C) 2014 Simon Bourne

#pragma once

#include "Enhedron/Util.h"

#include <memory>
#include <string>
#include <stdexcept>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Results {
    using std::exception;
    using std::string;
    using std::unique_ptr;
    using std::make_unique;
    using std::ostream;
    using std::endl;

    class ResultTest: public NoCopy {
    public:
        virtual ~ResultTest() {}
        virtual void passCheck(const string& description) = 0;
        virtual void failCheck(const string& description) = 0;
        virtual void failByException(const exception& e) = 0;
    };

    class ResultSimpleTest: public ResultTest {};

    class ResultGWTTest: public ResultTest {
    public:
        virtual ~ResultGWTTest() {}

        virtual Finally init() = 0;
        virtual void given(const string& text) = 0;
        virtual Finally when(const string& text) = 0;
        virtual Finally then(const string& text) = 0;
    };

    class ResultContext: public NoCopy {
    public:
        virtual ~ResultContext() {}
        virtual unique_ptr<ResultContext> child(const string& name) = 0;
        virtual void listTest(const string& name) = 0;
        virtual unique_ptr<ResultSimpleTest> simpleTest(const string& name) = 0;
        virtual unique_ptr<ResultGWTTest> gwtTest(const string& name) = 0;
    };

    class Results: public ResultContext {
    public:
        virtual ~Results() {}
        virtual void finish(uint64_t ran, uint64_t failed) = 0;
    };


    class HumanResultTest final: public NoCopy {
        Out<ostream> outputStream_;

        void indent() {
            (*outputStream_) << "    ";
        }
    public:
        HumanResultTest(Out<ostream> outputStream) :
                outputStream_(outputStream)
        {}

        void describe(const string& text) {
            indent();
            indent();
            (*outputStream_) << text << endl;
        }

        void spec(const string& text) {
            indent();
            indent();
            (*outputStream_) << text << endl;
        }

        void passCheck(const string& description) {}

        void failCheck(const string& description) {
            indent();
            (*outputStream_) << "TEST FAILED! " << description << endl;
        }

        void failByException(const exception& e) {
            indent();
            (*outputStream_) << "TEST FAILED! Exception: " << e.what() << endl;
        }
    };

    class HumanResultGWTTest: public ResultGWTTest {
        HumanResultTest test;
    public:
        HumanResultGWTTest(Out<ostream> outputStream) :
            test(outputStream)
        {}

        virtual Finally init() override {
            return Finally::empty();
        }

        virtual void given(const string& text) override {
            test.spec("Given " + text + ",");
        }

        virtual Finally when(const string& text) override {
            test.spec("when " + text + ",");
            return Finally::empty();
        }

        virtual Finally then(const string& text) override {
            test.spec("then " + text + ".");
            return Finally::empty();
        }

        virtual void passCheck(const string& description) override {}

        virtual void failCheck(const string& description) override {
            test.failCheck(description);
        }

        virtual void failByException(const exception& e) override {
            test.failByException(e);
        }
    };

    class HumanResultSimpleTest final: public ResultSimpleTest {
        HumanResultTest test;
    public:
        HumanResultSimpleTest(Out<ostream> outputStream) :
            test(outputStream)
        {}

        virtual void passCheck(const string& description) override {}

        virtual void failCheck(const string& description) override {
            test.failCheck(description);
        }

        virtual void failByException(const exception& e) override {
            test.failByException(e);
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

        virtual unique_ptr<ResultSimpleTest> simpleTest(const string& name) override {
            writeTestName(name);
            return make_unique<HumanResultSimpleTest>(outputStream_);
        }

        virtual unique_ptr<ResultGWTTest> gwtTest(const string& name) override {
            writeTestName(name);
            return make_unique<HumanResultGWTTest>(outputStream_);
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
    using Impl::Impl_Results::ResultSimpleTest;
    using Impl::Impl_Results::ResultTest;
    using Impl::Impl_Results::ResultGWTTest;
    using Impl::Impl_Results::HumanResults;
}}

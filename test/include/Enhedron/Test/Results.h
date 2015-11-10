// Copyright (C) 2014 Simon Bourne

#pragma once

#include "Enhedron/Util.h"
#include "Enhedron/Log.h"
#include "Enhedron/Util/Enum.h"
#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Assertion.h"
#include "Enhedron/Test/Module.h"
#include "Enhedron/Container/StringTree.h"

#include <boost/optional/optional.hpp>

#include <memory>
#include <string>
#include <stdexcept>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Results {
    using LogBlock = Log::BlockLogger<Log::DefaultWriter>;

    using std::exception;
    using std::string;
    using std::unique_ptr;
    using std::make_unique;

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

    class DefaultResultGWTTest: public ResultGWTTest {
        LogBlock block;
    public:
        DefaultResultGWTTest(const string& name) :
                block(log.infoBlock("gwtTest", "name", name))
        {}

        virtual Finally init() override {
            return Finally::wrap(log.infoBlock("given"));
        }

        virtual void given(const string& text) override {
            log.info("given", "text", text);
        }

        virtual Finally when(const string& text) override {
            return Finally::wrap(log.infoBlock("when", "text", text));
        }

        virtual Finally then(const string& text) override {
            return Finally::wrap(log.infoBlock("then", "text", text));
        }

        virtual void passCheck(const string& description) override {}

        virtual void failCheck(const string& description) override {
            log.error("failed", "description", description);
        }

        virtual void failByException(const exception& e) override {
            log.error("exception", "exception", e.what());
        }
    };

    class DefaultResultSimpleTest final: public ResultSimpleTest {
    public:
        LogBlock block;
    public:
        DefaultResultSimpleTest(const string& name) :
                block(log.infoBlock("simpleTest", "name", name))
        {}

        virtual void passCheck(const string& description) override {}

        virtual void failCheck(const string& description) override {
            log.error("failed", "description", description);
        }

        virtual void failByException(const exception& e) override {
            log.error("exception", "exception", e.what());
        }
    };

    template<typename Base>
    class DefaultResultContext: public Base {
        LogBlock block;
    public:
        DefaultResultContext(const string& name) : block(log.infoBlock("context", "name", name)) {}

        virtual unique_ptr<ResultContext> child(const string& name) {
            return make_unique<DefaultResultContext<ResultContext>>(name);
        }

        virtual void listTest(const string& name) override {
            log.info("testCase", "name", name);
        }

        virtual unique_ptr<ResultSimpleTest> simpleTest(const string& name) {
            return make_unique<DefaultResultSimpleTest>(name);
        }

        virtual unique_ptr<ResultGWTTest> gwtTest(const string& name) {
            return make_unique<DefaultResultGWTTest>(name);
        }
    };

    class DefaultResults final: public DefaultResultContext<Results> {
    public:
        DefaultResults() : DefaultResultContext("root") {}

        virtual void finish(uint64_t ran, uint64_t failed) override {
            uint64_t passed = ran - failed;

            if (failed != 0) {
                log.error("testsFailed", "message", "SOME TESTS FAILED!!!", "ran", ran, "passed", passed, "failed", failed);
            }
            else {
                log.info("OK", "ran", ran, "passed", passed, "failed", failed);
            }
        }
    private:
        Finally logBlock(const char* type, const string& name) {
            return Finally::wrap(log.infoBlock(type, "name", name));
        }
    };
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Results::Results;
    using Impl::Impl_Results::ResultContext;
    using Impl::Impl_Results::ResultSimpleTest;
    using Impl::Impl_Results::ResultTest;
    using Impl::Impl_Results::ResultGWTTest;
    using Impl::Impl_Results::DefaultResults;
}}

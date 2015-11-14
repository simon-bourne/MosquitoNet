#pragma once

#include "Enhedron/Test.h"
#include "Enhedron/Util/Main.h"
#include "Enhedron/Container/StringTree.h"

#include <string>
#include <vector>
#include <utility>

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/program_options.hpp>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Harness {
    using std::string;
    using std::move;
    using std::vector;
    using std::runtime_error;

    using boost::program_options::options_description;
    using boost::replace_all_copy;
    using boost::algorithm::is_any_of;
    using boost::algorithm::split;

    using Util::ExitStatus;
    using ::Enhedron::Util::Options;

    using ::Enhedron::Container::StringTree;

    namespace po = boost::program_options;

    namespace option {
        static constexpr const auto list = "list";
        static constexpr const auto runTest = "run_test";
    }

    options_description buildOptions() {
        po::options_description desc;

        desc.add_options()
                (option::list, "List tests instead of running them.")
                (option::runTest, po::value<string>(), "Run a particular test. Overrides positional arguments for tests.")
                ;

        return move(desc);
    }

    ExitStatus runTests(Options options) {
        vector<string> pathList;

        auto optionalSpecifyTest = options.optional<string>(option::runTest);

        if ( ! optionalSpecifyTest) {
            pathList = options.getPositional<string>();
        }

        StringTree pathTree;

        for (auto& path : pathList) {
            vector<string> pathComponentList;
            split(pathComponentList, path, is_any_of("."));
            auto childPtr = out(pathTree);

            for (const auto& pathComponent : pathComponentList) {
                auto child = childPtr->get(pathComponent);

                if (child) {
                    childPtr = *child;
                }
                else {
                    childPtr = childPtr->set(pathComponent);
                }
            }

            childPtr->clear();
        }

        if (options.flag(option::list)) {
            Test::list(pathTree);
        }
        else {
            if ( ! Test::run(pathTree)) {
                return ExitStatus::SoftwareError;
            }
        }

        return ExitStatus::Ok;
    }

    int run(int argc, const char* argv[]) {
        return main(runTests, buildOptions, "TestHarness", "TEST PATH", argc, argv);
    }
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Harness::run;
}}

// Copyright (C) 2014 Simon Bourne

#pragma once

#include <utility>
#include <iostream>
#include <stdexcept>
#include <vector>
#include <sysexits.h>
#include <string>
#include <sys/ioctl.h>
#include <cstdio>
#include <unistd.h>
#include <iosfwd>
#include <memory>
#include <mutex>

#include <boost/optional.hpp>
#include <boost/program_options.hpp>

#include "Enhedron/Log.h"
#include "Enhedron/Assertion.h"
#include "Enhedron/Util.h"

namespace Enhedron { namespace Util { namespace Impl { namespace Main {
    using std::ostringstream;
    using std::bad_alloc;
    using std::exception;
    using std::runtime_error;
    using std::logic_error;
    using std::cout;
    using std::cerr;
    using std::endl;
    using std::string;
    using std::forward;
    using std::move;
    using std::vector;
    using std::unique_ptr;
    using std::make_unique;
    using std::unique_lock;
    using std::mutex;

    using boost::optional;
    using boost::make_optional;
    using boost::none;

    namespace po = boost::program_options;

    static Logger log("Main");

    enum class ExitStatus {
        Ok              = EX_OK,
        OutOfMemory     = 1,
        InvalidOptions  = EX_USAGE,
        InternalError   = EX_SOFTWARE
    };

    class InvalidOptions final: public exception {
    };

    class Options {
    public:
        template<typename Value>
        Value required(const char* name) const {
            if (vm.count(name)) {
                return vm[name].as<Value>();
            }

            ostringstream message;
            message << "Required option " << name << " is missing.";
            commandLineError(message.str());

            throw logic_error("Shouldn't ever get here");
        }

        template<typename Value>
        optional<Value> optional(const char* name) const {
            if (vm.count(name)) {
                return make_optional(vm[name].as<Value>());
            }

            return none;
        }

        template<typename Value>
        vector<Value> getPositional() const {
            if (vm.count(positionalName)) {
                return vm[positionalName].as<vector<Value>>();
            }

            return vector<string>();
        }

        bool flag(const char* name) const {
            return vm.count(name);
        }

        template<typename... ParameterList>
        void raise(const string& message, const ParameterList&... parameterList) {
            commandLineError(message, parameterList...);
        }

        static unsigned int getTerminalColumns() {
            unsigned int terminalColumns = 80;

            #ifdef TIOCGSIZE
                struct ttysize ts;
                ioctl(STDIN_FILENO, TIOCGSIZE, &ts);
                terminalColumns = ts.ts_cols;
            #elif defined(TIOCGWINSZ)
                struct winsize ts;
                ioctl(STDIN_FILENO, TIOCGWINSZ, &ts);
                terminalColumns = ts.ws_col;
            #endif /* TIOCGSIZE */

            return terminalColumns;
        }

        //! @return none if help is specified, or Options
        template<typename OptionsBuilderFunctor>
        static boost::optional<Options> build(OptionsBuilderFunctor buildOptions, const char* positionalOptionsDescription, int argc, const char* argv[]) {
            try {
                const char* name = "unknown";

                if (argv && argv[0]) {
                    name = argv[0];
                }

                ostringstream summary;
                summary << name << " [OPTIONS]";

                if (positionalOptionsDescription) {
                    summary << " [" << positionalOptionsDescription << "]...";
                }

                static constexpr const char* logLevelOption = "log-level";
                auto logLevelOptionDesc = "Set the log level [" + Log::Level::getNameHelp() + "].";

                po::options_description helpDescription(summary.str(), getTerminalColumns());
                helpDescription.add_options()
                            ("help,h", "Show this message.")
                            (logLevelOption, po::value<string>(), logLevelOptionDesc.c_str())
                 ;

                helpDescription.add(buildOptions());

                po::options_description desc;

                if (positionalOptionsDescription) {
                    po::options_description positionalDesc;

                    positionalDesc.add_options()
                        (positionalName, po::value<vector<string>>()->composing(), positionalOptionsDescription)
                    ;

                    desc.add(positionalDesc);
                }

                desc.add(helpDescription);

                po::variables_map vm;
                po::positional_options_description positionalOptions;

                if (positionalOptionsDescription) {
                    positionalOptions.add(positionalName, -1);
                }

                Log::Global::init(Log::Level::verbose());

                po::store(
                          po::command_line_parser(argc, argv).
                          options(desc).
                          positional(positionalOptions).
                          run(),
                          vm
                    );

                if (vm.count("help") != 0) {
                    // Notify checks for errors and calls any registered notifiers,
                    // which we don't want to do if the help option is present.
                    cout << helpDescription << endl;

                    return none;
                }

                po::notify(vm);

                Options options(move(vm));

                auto logLevel = options.optional<string>(logLevelOption);

                if (logLevel) {
                    Log::Global::init(Log::Level::fromString(*logLevel));
                }

                return move(options);
            }
            catch (exception& e) {
                commandLineError(e.what());
            }

            throw InvalidOptions{};
        }
    private:
        static constexpr const char* positionalName = "__positional";

        Options(po::variables_map&& vm) :
            vm(move(vm))
        {}

        template<typename... ParameterList>
        static void commandLineError(const string& message, const ParameterList&... parameterList) {
            cerr << "\n\n" << message << "\n";
            cerr << "For more information, specify --help\n\n" << endl;
            log.error("badCommandLine", "message", message, parameterList...);

            throw InvalidOptions{};
        }

        po::variables_map vm;
    };

    template<typename RunFunctor, typename OptionsBuilderFunctor>
    int main(RunFunctor run, OptionsBuilderFunctor buildOptions, const char* name, int argc, const char* argv[]) {
        return main(move(run), move(buildOptions), name, nullptr, argc, argv);
    }

    template<typename RunFunctor, typename OptionsBuilderFunctor>
    int main(RunFunctor run, OptionsBuilderFunctor buildOptions, const char* name, const char* positionalOptionsDescription, int argc, const char* argv[]) {
        try {
            auto options(Options::build(buildOptions, positionalOptionsDescription, argc, argv));

            if ( ! options) {
                return static_cast<int>(ExitStatus::Ok);
            }

            try {
                run(move(*options));
            }
            catch (const bad_alloc& e) {
                log.error("badAlloc", "message", e.what());

                return static_cast<int>(ExitStatus::OutOfMemory);
            }
            catch (const exception& e) {
                log.error("unhandledException", "message", e.what());

                return static_cast<int>(ExitStatus::InternalError);
            }
        }
        catch (const InvalidOptions& e) {
            return static_cast<int>(ExitStatus::InvalidOptions);
        }
        catch (const exception& e) {
            cerr << "Uncaught exception initialising: " << e.what() << endl;

            return static_cast<int>(ExitStatus::InternalError);
        }

        return static_cast<int>(ExitStatus::Ok);
    }
}}}}

namespace Enhedron { namespace Util {
    using Impl::Main::Options;
    using Impl::Main::main;
}}

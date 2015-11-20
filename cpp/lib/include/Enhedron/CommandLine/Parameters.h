#include "Enhedron/Util.h"
#include "Enhedron/Util/MetaProgramming.h"
#include "Enhedron/Util/Optional.h"

#include <string>
#include <ostream>
#include <vector>
#include <set>
#include <map>
#include <algorithm>
#include <iterator>
#include <stdexcept>
#include <utility>
#include <iostream>

namespace Enhedron { namespace CommandLine { namespace Impl { namespace Impl_Parameters {
    using Util::bindFirst;
    using Util::optional;
    using Util::mapParameterPack;

    using std::string;
    using std::ostream;
    using std::vector;
    using std::set;
    using std::map;
    using std::string;
    using std::move;
    using std::back_inserter;
    using std::logic_error;
    using std::index_sequence_for;
    using std::forward;
    using std::cerr;
    using std::runtime_error;
    using std::exception;

    enum class ExitStatus {
        OK,
        USAGE = 64,   // command line usage error 
        DATAERR = 65,    // data format error 
        NOINPUT = 66,    // cannot open input 
        NOUSER = 67,    // addressee unknown 
        NOHOST = 68,    // host name unknown 
        UNAVAILABLE = 69,    // service unavailable 
        SOFTWARE = 70,    // internal software error 
        OSERR = 71,    // system error (e.g., can't fork) 
        OSFILE = 72,    // critical OS file missing 
        CANTCREAT = 73,    // can't create (user) output file 
        IOERR = 74,    // input/output error 
        TEMPFAIL = 75,    // temp failure; user is invited to retry 
        PROTOCOL = 76,    // remote error in protocol 
        NOPERM = 77,    // permission denied 
        CONFIG = 78    // configuration error 
    };

    static const string helpOption{"--help"};
    static const string versionOption{"--version"};

    class ParamName: public NoCopy {
        optional<string> shortName_;
        string longName_;
        optional<string> description_;
    public:
        ParamName(string longName) : longName_("--" + longName) {}
        ParamName(string longName, string description) : longName_("--" + longName), description_(move(description)) {}

        ParamName(char shortName, string longName) :
                shortName_("-"), longName_("--" + longName)
        {
            shortName_->push_back(shortName);
        }

        ParamName(char shortName, string longName, string description) :
                shortName_("-"), longName_("--" + longName), description_(move(description))
        {
            shortName_->push_back(shortName);
        }

        template<typename Functor>
        void forEachName(Functor&& functor) const {
            if (shortName_) {
                functor(*shortName_);
            }

            functor(longName_);
        }

        const string& longName() const { return longName_; }

        void showNames(Out<ostream> output) const {
            *output << "  ";

            if (shortName_) {
                *output << *shortName_ << ", ";
            }

            *output << longName_;
        }

        void showDescription(Out<ostream> output, size_t terminalWidth) const {
            if (description_) {
                *output << "               " << *description_;
            }

            *output << "\n";
        }
    };

    template<typename ValueType>
    class Option final: public ParamName {
    public:
        using Value = ValueType;
        using ParamName::ParamName;
    };

    class Flag final: public ParamName {
    public:
        using ParamName::ParamName;
    };

    enum class ParamType {
        OPTION,
        FLAG
    };

    class Arguments final : public NoCopy {
        Out<ostream> output_;
        string description_;
        string notes_;
        size_t terminalWidth_;

        template <typename... Params>
        void displayHelp(
                Out<ostream> output,
                const char *exeName,
                Params&&... params
        ) {
            *output << "Usage: " << exeName << " [OPTION]...\n\n";
            if ( ! description_.empty()) {
                *output << description_ << "\n\n";
            }

            mapParameterPack(
                    [this, output](const auto &arg) {
                        arg.showNames(output);
                        arg.showDescription(output, terminalWidth_);
                    },
                    params...
            );

            *output << "  --help        Display this help message.\n"
                    << "  --version     Display version information.\n\n";

            if ( ! notes_.empty()) {
                *output << notes_ << "\n\n";
            }
        }

        template<typename Functor>
        ExitStatus runImpl(
                map<string, vector<string>> optionValues,
                vector<string> positionalArgs,
                set<string> setFlags,
                Functor&& functor
            )
        {
            return functor(move(positionalArgs));
        }

        template<typename Functor, typename... ParamTail>
        ExitStatus runImpl(
                map<string, vector<string>> optionValues,
                vector<string> positionalArgs,
                set<string> setFlags,
                Functor&& functor,
                Option<string>&& param,
                ParamTail&&... paramTail
        )
        {
            vector<string> paramValues;

            param.forEachName([&] (const string& name) {
                const auto& newValues = optionValues[name];
                paramValues.insert(paramValues.end(), newValues.begin(), newValues.end());
            });

            if (paramValues.empty()) {
                *output_ << "Error: No value for " + param.longName() << "\n";
            }

            if (paramValues.size() > 1) {
                *output_ << "Error: Multiple values for " + param.longName() << "\n";
            }

            return runImpl(
                    move(optionValues),
                    move(positionalArgs),
                    move(setFlags),
                    bindFirst(
                            forward<Functor>(functor),
                            move(paramValues.front()),
                            index_sequence_for<Option<string>, ParamTail...>()
                    ),
                    forward<ParamTail>(paramTail)...
            );
        }

        template<typename Functor, typename... ParamTail>
        ExitStatus runImpl(
                map<string, vector<string>> optionValues,
                vector<string> positionalArgs,
                set<string> setFlags,
                Functor&& functor,
                Flag&& flag,
                ParamTail&&... paramTail
        )
        {
            bool flagValue = false;

            flag.forEachName([&] (const string& name) {
                flagValue = setFlags.count(name) > 0;
            });

            return runImpl(
                    move(optionValues),
                    move(positionalArgs),
                    move(setFlags),
                    bindFirst(
                            forward<Functor>(functor),
                            flagValue,
                            index_sequence_for<Flag, ParamTail...>()
                    ),
                    forward<ParamTail>(paramTail)...
            );
        }

        template<typename OptionType, typename... ParamsTail>
        void readNamesImpl(
                Out<set<string>> optionNames,
                Out<set<string>> allNames,
                const Option<OptionType>& option,
                const ParamsTail&... paramsTail
        )
        {
            option.forEachName([&] (const string& name) {
                optionNames->emplace(name);
            });

            readNames(optionNames, allNames, paramsTail...);
        }

        template<typename... ParamsTail>
        void readNamesImpl(
                Out<set<string>> optionNames,
                Out<set<string>> allNames,
                const Flag& flag,
                const ParamsTail&... paramsTail
        )
        {
            readNames(optionNames, allNames, paramsTail...);
        }

        void readNames(Out<set<string>> optionNames, Out<set<string>> allNames) {}

        template<typename ParamType, typename... ParamsTail>
        void readNames(
                Out<set<string>> optionNames,
                Out<set<string>> allNames,
                const ParamType& param,
                const ParamsTail&... paramsTail
        )
        {
            param.forEachName([&] (const string& name) {
                if ( ! allNames->emplace(name).second) {
                    throw logic_error("Duplicate name " + name);
                }
            });

            readNamesImpl(optionNames, allNames, param, paramsTail...);
        }

        bool checkArgs(int argc, const char* const argv[]) {
            if (argc <= 0) {
                throw runtime_error("argc is 0.");
            }
            else if (argv == nullptr) {
                throw runtime_error("argv is null.");
            }
            else {
                for (int index = 0; index < argc; ++index) {
                    if (argv[index] == nullptr) {
                        throw runtime_error("argv has null value.");
                    }
                    else if (argv[index] == helpOption) {
                        return true;
                    }
                }
            }

            return false;
        }

        template<typename Functor, typename... Params>
        ExitStatus runImpl(
                int argc, const char* const argv[],
                Functor &&functor,
                Params&&... params
        )
        {
            if (checkArgs(argc, argv)) {
                displayHelp(output_, argv[0], forward<Params>(params)...);
                return ExitStatus::OK;
            }

            set<string> optionNames;
            set<string> allNames;
            readNames(out(optionNames), out(allNames), params...);

            map<string, vector<string>> optionValues;
            vector<string> positionalArgs;
            set<string> setFlags;

            for (int index = 1; index < argc; ++index) {
                string currentArg(argv[index]);

                if (currentArg == "--") {
                    positionalArgs.insert(positionalArgs.end(), argv + index, argv + argc);
                    break;
                }

                if ( ! currentArg.empty() && currentArg[0] == '-') {
                    if (allNames.count(currentArg) == 0) {
                        *output_<< "Error: Unknown option " << currentArg << "\n";

                        return ExitStatus::USAGE;
                    }

                    if (optionNames.count(currentArg)) {
                        ++index;

                        if (index == argc) {
                            *output_<< "Error: No value supplied for option " << currentArg << "\n";

                            return ExitStatus::USAGE;
                        }

                        optionValues[currentArg].emplace_back(argv[index]);
                    }
                    else {
                        setFlags.emplace(currentArg);
                    }
                }
                else {
                    positionalArgs.emplace_back(currentArg);
                }
            }

            return runImpl(
                move(optionValues),
                move(positionalArgs),
                move(setFlags),
                forward<Functor>(functor),
                forward<Params>(params)...
            );
        }
    public:
        Arguments(Out<ostream> output, string description, string notes, size_t terminalWidth = 80) :
                output_(output),
                description_(description),
                notes_(notes),
                terminalWidth_(terminalWidth)
        {}

        // TODO: Help to cout, errors to cerr.
        // TODO: Positional args description (run overloaded with positional name, wrap functor in something that
        // throws if it gets positional args).
        // TODO: Alignment of description.
        // TODO: Wrapping lines on description.
        Arguments(string description, string notes, size_t terminalWidth = 80) :
                Arguments(out(cerr), move(description), move(notes), terminalWidth) {}

        template<typename Functor, typename... Params>
        int run(
                int argc, const char* const argv[],
                Functor &&functor,
                Params&&... params
            )
        {
            try {
                return static_cast<int>(runImpl(argc, argv, forward<Functor>(functor), forward<Params>(params)...));
            }
            catch (const exception& e) {
                *output_ << ((argv && argc > 0 && argv[0]) ? argv[0] : "unknown") << ": " << e.what() << "\n";
            }

            return static_cast<int>(ExitStatus::SOFTWARE);
        }
    };
}}}}

namespace Enhedron { namespace CommandLine {
    using Impl::Impl_Parameters::ExitStatus;
    using Impl::Impl_Parameters::Arguments;
    using Impl::Impl_Parameters::Option;
    using Impl::Impl_Parameters::Flag;
}}

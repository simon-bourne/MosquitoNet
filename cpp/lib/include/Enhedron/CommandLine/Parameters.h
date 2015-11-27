//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "Enhedron/Util.h"
#include "Enhedron/Util/MetaProgramming.h"
#include "Enhedron/Util/Optional.h"
#include "Enhedron/Util/Math.h"

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
#include <sstream>
#include <iterator>

namespace Enhedron { namespace CommandLine { namespace Impl { namespace Impl_Parameters {
    using Util::bindFirst;
    using Util::optional;
    using Util::mapParameterPack;
    using Util::makeDivisibleByRoundingUp;

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
    using std::cout;
    using std::cerr;
    using std::runtime_error;
    using std::exception;
    using std::ostringstream;
    using std::min;
    using std::max;
    using std::fill_n;
    using std::ostream_iterator;
    using std::copy;

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

    class Name: public NoCopy {
        optional<string> shortName_;
        string longName_;
        optional<string> description_;
    public:
        Name(string longName) : longName_("--" + longName) {}
        Name(string longName, string description) : longName_("--" + longName), description_(move(description)) {}

        Name(char shortName, string longName) :
                shortName_("-"), longName_("--" + longName)
        {
            shortName_->push_back(shortName);
        }

        Name(char shortName, string longName, string description) :
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

        bool anyMatch(const char* arg) const {
            if (shortName_) {
                if (arg == *shortName_) {
                    return true;
                }
            }

            return arg == longName_;
        }

        const string& longName() const { return longName_; }

        string makeNamesString() const {
            string result("  ");

            if (shortName_) {
                result += *shortName_ + ", ";
            }

            return result + longName_;
        }

        bool multiLineDescription(size_t width) const {
            return description_ && description_->size() > width;
        }

        void showDescription(Out<ostream> output, size_t width, size_t padding) const {
            width = max(width, static_cast<size_t>(10u));

            if (description_) {
                auto current = description_->begin();

                while (static_cast<size_t>(description_->end() - current) > width) {
                    auto currentEnd = current + static_cast<string::difference_type>(width);
                    auto breakAt = currentEnd;

                    while (true) {
                        --breakAt;

                        if (breakAt == current) {
                            // We didn't find a space - hyphenate.
                            --currentEnd;
                            copy(current, currentEnd, ostream_iterator<char>(*output));
                            *output << "-";
                            current = currentEnd;
                            break;
                        }

                        if (*breakAt == ' ') {
                            copy(current, breakAt, ostream_iterator<char>(*output));
                            current = breakAt;

                            break;
                        }
                    }

                    *output << "\n";
                    fill_n(ostream_iterator<char>(*output), padding, ' ');

                    while (*current == ' ') {
                        ++current;
                    }
                }

                copy(current, description_->end(), ostream_iterator<char>(*output));
            }

            *output << "\n";
        }
    };

    template<typename ValueType>
    class Option final {
        Name name_;
        string valueName_;
        optional<string> defaultValue_;
    public:
        using Value = ValueType;

        Option(Name name, string valueName) : name_(move(name)), valueName_(move(valueName)) {}

        Option(Name name, string valueName, string defaultValue) :
                name_(move(name)), valueName_(move(valueName)), defaultValue_(move(defaultValue)) {}

        template<typename Functor>
        void forEachName(Functor&& functor) const {
            name_.forEachName(forward<Functor>(functor));
        }

        bool anyMatch(const char* arg) const {
            return name_.anyMatch(arg);
        }

        const string& longName() const { return name_.longName(); }

        string makeNamesString() const {
            return name_.makeNamesString() + " <" + valueName_ + ">";
        }

        bool multiLineDescription(size_t width) const {
            return name_.multiLineDescription(width);
        }

        void showDescription(Out<ostream> output, size_t width, size_t padding) const {
            name_.showDescription(output, width, padding);
        }

        optional<string> defaultValue() const { return defaultValue_; }
    };

    class Flag final: public Name {
    public:
        using Name::Name;
    };

    enum class ParamType {
        OPTION,
        FLAG
    };

    class Arguments final : public NoCopy {
        Out<ostream> helpOut_;
        Out<ostream> errorOut_;
        string description_;
        string notes_;
        string version_;
        string positionalDescription_;
        size_t terminalWidth_;

        static const Flag& helpFlag() {
            static const Flag instance{"help", "Display this help message."};
            return instance;
        }

        static const Flag& versionFlag() {
            static const Flag instance{"version", "Display version information."};
            return instance;
        }

        template <typename... Params>
        void displayHelp(
                const char *exeName,
                Params&&... params
        ) {
            *helpOut_ << "Usage: " << exeName << " [OPTION]...";

            if ( ! positionalDescription_.empty()) {
                *helpOut_ << " [" << positionalDescription_ << "]..."; 
            }
            
            *helpOut_ << "\n\n";
            
            if ( ! description_.empty()) {
                *helpOut_ << description_ << "\n\n";
            }

            size_t padding = 0;

            mapParameterPack(
                    [this, &padding](const auto &arg) {
                        padding = max(arg.makeNamesString().size(), padding);
                    },
                    params...,
                    helpFlag(),
                    versionFlag()
            );

            constexpr const size_t tabWidth = 4;
            padding += tabWidth;
            padding = makeDivisibleByRoundingUp(padding, tabWidth);
            padding = min(terminalWidth_ / 2, padding);

            mapParameterPack(
                    [this, padding, tabWidth](const auto &arg) {
                        auto nameString = arg.makeNamesString();
                        *helpOut_ << nameString;
                        size_t currentPadding = padding;
                        bool descriptionOnNewline = nameString.size() + tabWidth > padding;

                        if (descriptionOnNewline) {
                            *helpOut_ << "\n";
                        }
                        else {
                            currentPadding -= nameString.size();
                        }

                        fill_n(ostream_iterator<char>(*helpOut_), currentPadding, ' ');
                        auto descriptionWidth = terminalWidth_ - padding;

                        arg.showDescription(helpOut_, descriptionWidth, padding);

                        if (descriptionOnNewline || arg.multiLineDescription(descriptionWidth)) {
                            *helpOut_ << "\n";
                        }
                    },
                    params...,
                    helpFlag(),
                    versionFlag()
            );

            *helpOut_ << "\n";

            if ( ! notes_.empty()) {
                *helpOut_ << notes_ << "\n\n";
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

            string value;

            if (paramValues.empty()) {
                if (param.defaultValue()) {
                    value = *param.defaultValue();
                }
                else {
                    *errorOut_ << "Error: No value for " + param.longName() << "\n";

                    return ExitStatus::CONFIG;
                }
            }
            else {
                value = paramValues.front();
            }

            if (paramValues.size() > 1) {
                *errorOut_ << "Error: Multiple values for " + param.longName() << "\n";

                return ExitStatus::CONFIG;
            }

            return runImpl(
                    move(optionValues),
                    move(positionalArgs),
                    move(setFlags),
                    bindFirst(
                            forward<Functor>(functor),
                            move(value),
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
                Option<vector<string>>&& param,
                ParamTail&&... paramTail
        )
        {
            vector<string> paramValues;

            param.forEachName([&] (const string& name) {
                const auto& newValues = optionValues[name];
                paramValues.insert(paramValues.end(), newValues.begin(), newValues.end());
            });

            return runImpl(
                    move(optionValues),
                    move(positionalArgs),
                    move(setFlags),
                    bindFirst(
                            forward<Functor>(functor),
                            move(paramValues),
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
                flagValue |= setFlags.count(name) > 0;
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

        enum class StandardArg {
            NONE,
            HELP,
            VERSION
        };

        StandardArg checkArgs(int argc, const char* const argv[]) {
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
                    else {
                        if (helpFlag().anyMatch(argv[index])) {
                            return StandardArg::HELP;
                        }

                        if (versionFlag().anyMatch(argv[index])) {
                            return StandardArg::VERSION;
                        }
                    }
                }
            }

            return StandardArg::NONE;
        }

        template<typename Functor, typename... Params>
        ExitStatus runImpl(
                int argc, const char* const argv[],
                Functor &&functor,
                Params&&... params
        )
        {
            auto standardArg = checkArgs(argc, argv);

            if (standardArg == StandardArg::HELP) {
                displayHelp(argv[0], forward<Params>(params)...);
                return ExitStatus::OK;
            }

            if (standardArg == StandardArg::VERSION) {
                *helpOut_ << version_ << "\n";
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
                        *errorOut_<< "Error: Unknown option " << currentArg << "\n";

                        return ExitStatus::USAGE;
                    }

                    if (optionNames.count(currentArg)) {
                        ++index;

                        if (index == argc) {
                            *errorOut_<< "Error: No value supplied for option " << currentArg << "\n";

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

        const char* exeName(int argc, const char* const argv[]) {
            if (argv && argc > 0 && argv[0]) {
                return argv[0];
            }

            return "unknown";
        }
    public:
        Arguments(Out<ostream> helpOut, Out<ostream> errorOut, string version, size_t terminalWidth = 80) :
            helpOut_(helpOut), errorOut_(errorOut), version_(move(version)), terminalWidth_(terminalWidth)
        {}

        void setDescription(string description) { description_ = move(description); }
        void setNotes(string notes) { notes_ = move(notes); }
        void setPositionalDescription(string positionalDescription) {
            positionalDescription_ = move(positionalDescription);
        }

        Arguments(string version, size_t terminalWidth = 80) :
            Arguments(out(cout), out(cerr), move(version), terminalWidth)
        {}

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
                *errorOut_ << exeName(argc, argv) << ": " << e.what() << "\n";
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
    using Impl::Impl_Parameters::Name;
}}

// Simon Bourne 2014

#pragma once

#include <thread>
#include <ostream>
#include <sstream>
#include <cstring>
#include <type_traits>

#include <boost/date_time/posix_time/posix_time.hpp>

#include "Enhedron/Util.h"
#include "Enhedron/Util/Json.h"

namespace Enhedron { namespace Log { namespace Impl { namespace DefaultWriter {
    using std::string;

    struct EscapedString {
        const string& value;
    };

    struct EscapedCharString {
        const char* value;
    };
}}}}

namespace Enhedron { namespace Log {
    // We need to declare all specialisations before first use to get implicit instantiation.
    template<typename Value, typename Enable = void>
    struct WriteValue {
        static void writeValue(std::ostream& out, const Value& value) {
            std::ostringstream valueStream;
            valueStream << value;
            out << "\"" << Util::jsonEscape(valueStream.str()) << "\"";
        }
    };

    template<>
    struct WriteValue<bool> {
        static void writeValue(std::ostream& out, bool value) {
            out << (value ? "true" : "false");
        }
    };

    template<>
    struct WriteValue<char> {
        static void writeValue(std::ostream& out, char value) {
            out << int(value);
        }
    };

    template<>
    struct WriteValue<unsigned char> {
        static void writeValue(std::ostream& out, unsigned char value) {
            out << int(value);
        }
    };

    template<>
    struct WriteValue<signed char> {
        static void writeValue(std::ostream& out, signed char value) {
            out << int(value);
        }
    };

    template<>
    struct WriteValue<std::string> {
        static void writeValue(std::ostream& out, const std::string& value) {
            out << "\"" << Util::jsonEscape(value) << "\"";
        }
    };

    template<>
    struct WriteValue<const char*> {
        static void writeValue(std::ostream& out, const char* value) {
            out << "\"" << Util::jsonEscape(value) << "\"";
        }
    };

    template<>
    struct WriteValue<Impl::DefaultWriter::EscapedString> {
        static void writeValue(std::ostream& out, const Impl::DefaultWriter::EscapedString& escapedString) {
            out << "\"" << escapedString.value << "\"";
        }
    };

    template<>
    struct WriteValue<Impl::DefaultWriter::EscapedCharString> {
        static void writeValue(std::ostream& out, const Impl::DefaultWriter::EscapedCharString& escapedString) {
            out << "\"" << escapedString.value << "\"";
        }
    };

    template<typename Value>
    struct WriteValue<Value, typename std::enable_if<std::is_arithmetic<Value>::value>::type> {
        static void writeValue(std::ostream& out, Value value) {
            out << value;
        }
    };
}}

namespace Enhedron { namespace Log { namespace Impl { namespace DefaultWriter {
    using std::setfill;
    using std::setw;
    using std::ostringstream;
    using std::string;
    using std::ostream;
    using std::thread;

    namespace this_thread = std::this_thread;

    using boost::posix_time::microsec_clock;

    using Util::jsonEscape;

    class DefaultWriter final: public NoCopy {
    public:
        DefaultWriter(const char* module) :
            module(jsonEscape(module))
        {
        }

        /** @brief Write a log line.
         *
         * Thread safe
         * Log line will be in json e.g.
         *
         * { "utcTime": "2014-03-02 12:19:12.580420", "level": "Error", "threadId": "12345", "module": "MyModule", "message": "File not found", "file": "/home/myuser/myfile" }
         */
        template<typename... VarField>
        string write(Level level, EntryType type, const char* name, const VarField&... extraFields) {
            ostringstream outStream;
            thread_local string threadId = getStringThreadId();

            outStream << "{ ";

            outStream << "\"utcTime\": \"";
            writeUTCTime(out(outStream));
            outStream << "\", ";

            writeMember(outStream, "type", EscapedCharString{logEntryTypeToString(type)});
            writeComma(outStream);
            writeMember(outStream, "level", EscapedCharString{level.getName()});
            writeComma(outStream);
            writeMember(outStream, "levelId", static_cast<size_t>(level.getId()));
            writeComma(outStream);
            writeMember(outStream, "threadId", EscapedString{threadId});
            writeComma(outStream);
            writeMember(outStream, "module", EscapedString{module});
            writeComma(outStream);
            writeMember(outStream, "name", EscapedCharString{name});

            outStream << ", \"members\": { ";
            writeFields(outStream, extraFields...);

            outStream << " }}\n";

            return outStream.str();
        }

    private:
        static string getStringThreadId() {
            thread::id notThread;
            auto threadId = this_thread::get_id();

            if (threadId == notThread) {
                return "Main";
            }

            ostringstream threadIdStr;
            threadIdStr << threadId;

            return jsonEscape(threadIdStr.str());
        }

        void writeUTCTime(Out<ostream> out) {
            auto now = microsec_clock::universal_time();
            auto date = now.date();
            auto time = now.time_of_day();

            out.get() <<
                    setw(4) << setfill('0') << date.year() << "-" <<
                    setw(2) << setfill('0') << date.month().as_number() << "-" <<
                    setw(2) << setfill('0') << date.day().as_number() << "T" <<
                    setw(2) << setfill('0') << time.hours() << ":" <<
                    setw(2) << setfill('0') << time.minutes() << ":" <<
                    setw(2) << setfill('0') << time.seconds() << "." <<
                    setw(6) << setfill('0') << time.fractional_seconds();

        }

        inline void writeComma(ostream& out) {
            out << ", ";
        }

        template<typename NAME, typename VALUE>
        inline void writeMember(ostream& out, const NAME& name, const VALUE& value) {
            out << "\"" << name << "\": ";
            WriteValue<VALUE>::writeValue(out, value);
        }

        void writeFields(ostream&) {
        }

        template<typename ValueType, typename... FieldList>
        void writeFields(ostream& out, const char* fieldName, const ValueType& fieldValue, const FieldList&... extraFields) {
            writeMember(out, fieldName, fieldValue);
            writeAdditionalFields(out, extraFields...);
        }

        void writeAdditionalFields(ostream&) {
        }

        template<typename ValueType, typename... FieldList>
        void writeAdditionalFields(ostream& out, const char* fieldName, const ValueType& fieldValue, const FieldList&... extraFields) {
            writeComma(out);
            writeMember(out, fieldName, fieldValue);
            writeAdditionalFields(out, extraFields...);
        }

        string module;
    };
}}}}

namespace Enhedron { namespace Log {
    using Impl::DefaultWriter::DefaultWriter;
}}

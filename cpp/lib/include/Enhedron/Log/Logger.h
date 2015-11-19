// Copyright Simon Bourne 2014
#pragma once

#include <mutex>
#include <stdexcept>
#include <chrono>
#include <iostream>
#include <string>
#include <mutex>
#include <atomic>
#include <utility>

#include "Enhedron/Util.h"

namespace Enhedron { namespace Log { namespace Impl { namespace Logger {
    using std::chrono::duration_cast;
    using std::chrono::duration;
    using std::chrono::high_resolution_clock;
    using std::runtime_error;
    using std::cout;
    using std::string;
    using std::mutex;
    using std::unique_lock;
    using std::move;
    using std::atomic;
    using std::forward;

    class Global: public NoCopy {
    public:
        Global() = delete;

        static void init(Level level) {
            getLevelId() = level.getId();
        }

        static bool enabled(Level level) {
            return getLevelId() >= level.getId();
        }

        static unique_lock<mutex> lock() {
            static mutex logMutex;

            return unique_lock<mutex>(logMutex);
        }

    private:
        static atomic<Level::Id>& getLevelId() {
            static atomic<Level::Id> levelId(Level::info().getId());
            return levelId;
        }
    };

    template<typename Writer, typename... Args>
    void writeLogLine(Out<Writer> writer, Level level, EntryType entryType, const char* context, Args&&... args) {
        auto line(writer->write(level, entryType, context, forward<Args>(args)...));

        if (Global::enabled(level)) {
            auto lock(Global::lock());
            cout << line;
            cout.flush();
        }
    }

    template<typename Writer>
    class BlockLogger final: public NoCopy {
    public:
        template<typename... Args>
        BlockLogger(Out<Writer> writer, Level level, const char* context, Args&&... args)
        :
            level(level),
            writer(writer),
            context(context)
        {
            writeLogLine(writer, level, EntryType::Begin, context, forward<Args>(args)...);
            startTime = clock.now();
        }

        BlockLogger(BlockLogger<Writer>&& source) :
            level(source.level),
            clock(move(source.clock)),
            startTime(move(source.startTime)),
            writer(source.writer),
            context(move(source.context)),
            closed(source.closed)
        {
            source.closed = true;
        }

        BlockLogger<Writer>& operator=(BlockLogger<Writer>&& source) {
            level = source.level;
            clock = move(source.clock);
            startTime = move(source.startTime);
            writer = source.writer;
            context = move(source.context);
            closed = source.closed;
            source.closed = true;

            return *this;
        }

        ~BlockLogger() {
            close();
        }

        void close() {
            if ( ! closed) {
                auto endTime = clock.now();
                auto elapsedSeconds = duration_cast<duration<double>>(endTime - startTime);
                writeLogLine(writer, level, EntryType::End, context.c_str(), "elapsedSeconds", elapsedSeconds.count());
                closed = true;
            }
        }
    private:
        Level level;
        high_resolution_clock clock;
        high_resolution_clock::time_point startTime;
        Out<Writer> writer;
        string context;
        bool closed = false;
    };

    template<typename Writer>
    class ConfigurableLogger final: public NoCopy {
    public:
        ConfigurableLogger(const char* module) : writer(module) {}

        template<typename... Args>
        void error(const char* context, Args&&... args) {
            write(Level::error(), context, forward<Args>(args)...);
        }

        template<typename... Args>
        void warn(const char* context, Args&&... args) {
            write(Level::warn(), context, forward<Args>(args)...);
        }

        template<typename... Args>
        void info(const char* context, Args&&... args) {
            write(Level::info(), context, forward<Args>(args)...);
        }

        template<typename... Args>
        void verbose(const char* context, Args&&... args) {
            write(Level::verbose(), context, forward<Args>(args)...);
        }

        template<typename... Args>
        void trace(const char* context, Args&&... args) {
            write(Level::trace(), context, forward<Args>(args)...);
        }

        template<typename... Args>
        BlockLogger<Writer> errorBlock(const char* context, Args&&... args) {
            return move(BlockLogger<Writer>(out(writer), Level::error(), context, forward<Args>(args)...));
        }

        template<typename... Args>
        BlockLogger<Writer> warnBlock(const char* context, Args&&... args) {
            return move(BlockLogger<Writer>(out(writer), Level::warn(), context, forward<Args>(args)...));
        }

        template<typename... Args>
        BlockLogger<Writer> infoBlock(const char* context, Args&&... args) {
            return move(BlockLogger<Writer>(out(writer), Level::info(), context, forward<Args>(args)...));
        }

        template<typename... Args>
        BlockLogger<Writer> verboseBlock(const char* context, Args&&... args) {
            return move(BlockLogger<Writer>(out(writer), Level::verbose(), context, forward<Args>(args)...));
        }

        template<typename... Args>
        BlockLogger<Writer> traceBlock(const char* context, Args&&... args) {
            return move(BlockLogger<Writer>(out(writer), Level::trace(), context, forward<Args>(args)...));
        }

        template<typename... Args>
        void raiseIf(bool condition, const char* context, Args&&... args) {
            if (condition) {
                raise(context, forward<Args>(args)...);
            }
        }

        template<typename... Args>
        void raise(const char* context, Args&&... args) {
            error(context, forward<Args>(args)...);
            throw runtime_error("Error");
        }
    private:
        template<typename... Args>
        void write(Level level, const char* context, Args&&... args) {
            writeLogLine(out(writer), level, EntryType::Log, context, forward<Args>(args)...);
        }

        Writer writer;
    };
}}}}

namespace Enhedron { namespace Log {
    using Impl::Logger::Global;
    using Impl::Logger::ConfigurableLogger;
    using Impl::Logger::BlockLogger;
}}

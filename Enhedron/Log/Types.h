// Copyright Enhedron Ltd 2014

#pragma once

#include <stdexcept>
#include <vector>
#include <cassert>
#include <algorithm>
#include <locale>
#include <cctype>

#include "Enhedron/Util.h"

namespace Enhedron { namespace Log { namespace Impl { namespace Types {
	using std::runtime_error;
	using std::vector;
	using std::string;
	using std::tolower;
	using std::locale;
	using std::transform;

	class Level final {
	public:
		enum class Id {
			NONE,
			ERROR,
			WARN,
			INFO,
			VERBOSE,
			TRACE
		};

		Level() = delete;

		Id getId() const { return id; }

		const char* getName() const {
			switch (id) {
			case Id::NONE:
				return "None";
			case Id::ERROR:
				return "Error";
			case Id::WARN:
				return "Warn";
			case Id::INFO:
				return "Info";
			case Id::VERBOSE:
				return "";
			case Id::TRACE:
				return "Trace";
			}

			assert(false);

			return "Unknwon";
		}

		static Level none() {
			return Level(Id::NONE);
		}

		static Level error() {
			return Level(Id::ERROR);
		}

		static Level warn() {
			return Level(Id::WARN);
		}

		static Level info() {
			return Level(Id::INFO);
		}

		static Level verbose() {
			return Level(Id::VERBOSE);
		}

		static Level trace() {
			return Level(Id::TRACE);
		}

		static string getNameHelp() {
			const auto& levelList = getLevelList();

			if (levelList.empty()) {
				return "";
			}

			string nameHelp = levelList.front().getName();

			for (auto levelIter = levelList.begin() + 1; levelIter != levelList.end(); ++levelIter) {
				nameHelp += '|';
				nameHelp += levelIter->getName();
			}

			return nameHelp;
		}

		static Level fromString(const string& name) {
			string nameLower(name);
			toLowerInPlace(out(nameLower));

			const auto& levelList = getLevelList();

			for (const auto& level : levelList) {
				string compareString = level.getName();
				toLowerInPlace(out(compareString));

				if (compareString == nameLower) {
					return level;
				}
			}

			throw runtime_error("Unknown log level \"" + name +"\"");
		}
	private:
		static void toLowerInPlace(Out<string> s) {
			transform(s->begin(), s->end(), s->begin(), [](char c) { return tolower(c, locale()); } );
		}
		Level(Id id) : id(id) {}

		static const vector<Level>& getLevelList() {
			static vector<Level> levelList{ none(), error(), warn(), info(), verbose(), trace() };

			return levelList;
		}

		Id id;
	};

	enum class EntryType {
		Begin,
		End,
		Log
	};

	inline const char* logEntryTypeToString(EntryType type) {
		if (type == EntryType::Log) {
			return "log";
		}

		if (type == EntryType::Begin) {
			return "begin";
		}

		return "end";
	}
}}}}

namespace Enhedron { namespace Log {
	using Impl::Types::Level;
	using Impl::Types::EntryType;
}}


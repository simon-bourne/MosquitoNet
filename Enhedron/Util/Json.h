// Copyright (C) 2014 Simon Bourne

#pragma once

#include <string>

namespace Enhedron { namespace Util { namespace Impl { namespace Json {
    using std::string;

    inline string jsonEscape(const string& input) {
        string escapedString;
        escapedString.reserve(input.size());

        for (auto c : input) {
            if (c < 0x20) {
                char escapedChar[10]; // Should only ever need 7.
                sprintf(escapedChar, "\\u00%1x", (unsigned int) c);
                escapedString.append(escapedChar);
            }
            else {
                switch (c) {
                    case '\"':
                    case '\\':
                    case '\b':
                    case '\f':
                    case '\n':
                    case '\r':
                    case '\t':
                        escapedString.append("\\");
                        break;
                    default:
                        break;
                }

                escapedString += c;
            }
        }

        return escapedString;
    }
}}}}

namespace Enhedron { namespace Util {
    using Impl::Json::jsonEscape;
}}

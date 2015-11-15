// Copyright (C) 2014 Simon Bourne

#pragma once

#include <string>

namespace Enhedron { namespace Util { namespace Impl { namespace Json {
    using std::string;

    inline string jsonEscape(const string& input) {
        string escapedString;
        escapedString.reserve(input.size());

        for (auto c : input) {
            switch (c) {
                case '\"':
                    escapedString.append("\\");
                    escapedString += '\"';
                    break;
                case '\\':
                    escapedString.append("\\");
                    escapedString += '\\';
                    break;
                case '\b':
                    escapedString.append("\\");
                    escapedString += 'b';
                    break;
                case '\f':
                    escapedString.append("\\");
                    escapedString += 'f';
                    break;
                case '\n':
                    escapedString.append("\\");
                    escapedString += 'n';
                    break;
                case '\r':
                    escapedString.append("\\");
                    escapedString += 'r';
                    break;
                case '\t':
                    escapedString.append("\\");
                    escapedString += 't';
                    break;
                default:
                    if (c < 0x20) {
                        char escapedChar[10]; // Should only ever need 7.
                        sprintf(escapedChar, "\\u00%02x", static_cast<unsigned int>(c));
                        escapedString.append(escapedChar);
                    }
                    else {
                        escapedString += c;
                    }
                    break;
            }
        }

        return escapedString;
    }
}}}}

namespace Enhedron { namespace Util {
    using Impl::Json::jsonEscape;
}}

// Copyright (C) 2014 Simon Bourne

#ifndef ENHEDRON_UTIL_ENUM_H_
#define ENHEDRON_UTIL_ENUM_H_

#include <stdexcept>
#include <type_traits>

namespace Enhedron { namespace Util { namespace Impl { namespace Enum {
    using std::runtime_error;
    using std::underlying_type_t;

    /**
     * Convert a value to an enum safely. The enum must have "LAST_ENUM_VALUE" as the last
     * enumerated value, and must be a simple enum starting at 0 and being densely populated. For example:
     *
     *      enum class MyEnum {
     *          VALUE_0,
     *          VALUE_1,
     *          LAST_ENUM_VALUE = VALUE_1
     *      };
     */
    template<typename Enum, typename Value>
    Enum toEnum(Value value) {
        if (value >= 0 && value <= static_cast<underlying_type_t<Enum>>(Enum::LAST_ENUM_VALUE)) {
            return static_cast<Enum>(value);
        }

        throw runtime_error("Value out of range for enum");
    }

    /**
     * Tag a type with an enumerated value to create a distinct type. Useful to differentiate values of the same type.
     * For example, to stop first and last names being interchangeable in the type system:
     *      enum class Name {
     *          FIRST,
     *          LAST
     *      };
     *
     *      using FirstName = TaggedValue<Name, Name::FIRST, string>;
     *      using LastName = TaggedValue<Name, Name::LAST, string>;
     */
    template<typename Enum, Enum tag, typename Value>
    class TaggedValue final {
        Value value;
    public:
        TaggedValue(Value value) : value(move(value)) { }

        Value& operator*() { return value; }
        const Value& operator*() const { return value; }

        Value* operator->() { return &value; }
        const Value* operator->() const { return &value; }
    };
}}}}

namespace Enhedron { namespace Util {
    using Impl::Enum::toEnum;
    using Impl::Enum::TaggedValue;
}}

#endif /* ENHEDRON_UTIL_ENUM_H_ */

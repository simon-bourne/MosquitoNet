// Copyright (C) 2014 Simon Bourne

#pragma once

#include <limits>
#include <stdexcept>
#include <type_traits>

namespace Enhedron { namespace Impl { namespace Util {
    using std::runtime_error;
    using std::numeric_limits;
    using std::enable_if;
    using std::is_base_of;
    using std::underlying_type;

    class NoCopy {
    public:
        NoCopy() = default;

        NoCopy(NoCopy&&) = default;
        NoCopy& operator=(NoCopy&&) = default;

        NoCopy(const NoCopy&) = delete;
        NoCopy& operator=(const NoCopy&) = delete;
    };

    class NoCopyMove : public NoCopy {
    public:
        NoCopyMove() = default;

        NoCopyMove(NoCopyMove&&) = delete;
        NoCopyMove& operator=(NoCopyMove&&) = delete;
    };

    template<typename ValueType>
    class Out final {
    public:
        template<typename Super, typename enable_if<is_base_of<ValueType, Super>::value>::type* = nullptr>
        Out(Out<Super> value) : value(value.value) { }

        explicit Out(ValueType& value) : value(&value) { }

        ValueType& operator*() { return *value; }
        const ValueType& operator*() const { return *value; }

        ValueType* operator->() { return value; }
        const ValueType* operator->() const { return value; }

    private:
        template<typename Super>
        friend class Out;

        ValueType* value;
    };

    template<typename ValueType>
    Out<ValueType> out(ValueType& value) {
        return Out<ValueType>(value);
    }

    // Assumes numerator + denominator + 1 doesn't overflow.
    template<typename NumeratorType, typename DenominatorType>
    constexpr NumeratorType divideRoundingUp(NumeratorType numerator, DenominatorType denominator) {
        // Doesn't work for -ve numbers.
        static_assert(!numeric_limits<NumeratorType>::is_signed, "NumeratorType must be unsigned");
        static_assert(!numeric_limits<DenominatorType>::is_signed, "DenominatorType must be unsigned");
        return (numerator + denominator - NumeratorType(1u)) / denominator;
    }

    template<typename Value, typename Modulus>
    constexpr Value roundDownBy(Value value, Modulus modulus) {
        // Doesn't work for -ve numbers.
        static_assert(!numeric_limits<Value>::is_signed, "Value must be unsigned");
        static_assert(!numeric_limits<Modulus>::is_signed, "Modulus must be unsigned");
        return (value / modulus) * modulus;
    }

    template<typename Value, typename Modulus>
    constexpr Value roundUpBy(Value value, Modulus modulus) {
        // Doesn't work for -ve numbers.
        static_assert(!numeric_limits<Value>::is_signed, "Value must be unsigned");
        static_assert(!numeric_limits<Modulus>::is_signed, "Modulus must be unsigned");
        return ((value + modulus - 1) / modulus) * modulus;
    }

    //! Is numerator divisible by denominator
    template<typename NumeratorType, typename DenominatorType>
    constexpr bool isDivisible(NumeratorType numerator, DenominatorType denominator) {
        return (numerator / denominator) * denominator == numerator;
    }

    template<typename Enum, typename Value>
    Enum toEnum(Value value) {
        // underlying_type_t is only available since C++ 14
        if (value >= 0 && value <= static_cast<typename underlying_type<Enum>::type>(Enum::LastEnumValue)) {
            return static_cast<Enum>(value);
        }

        throw runtime_error("Value out of range for enum");
    }

    //! Explicitly state a parameter is unused.
    template<typename... Value>
    void unused(Value& ...) { }

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
}}}

namespace Enhedron {
    using Impl::Util::NoCopy;
    using Impl::Util::NoCopyMove;
    using Impl::Util::Out;
    using Impl::Util::out;
    using Impl::Util::divideRoundingUp;
    using Impl::Util::roundDownBy;
    using Impl::Util::roundUpBy;
    using Impl::Util::isDivisible;
    using Impl::Util::toEnum;
    using Impl::Util::unused;
    using Impl::Util::TaggedValue;
}

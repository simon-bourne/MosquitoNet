// Copyright (C) 2014 Simon Bourne

#pragma once

#include <limits>

namespace Enhedron { namespace Util { namespace Impl { namespace Math {
    using std::numeric_limits;

    // Assumes numerator + denominator + 1 doesn't overflow.
    template<typename NumeratorType, typename DenominatorType>
    constexpr NumeratorType divideRoundingUp(NumeratorType numerator, DenominatorType denominator) {
        // Doesn't work for -ve numbers.
        static_assert(!numeric_limits<NumeratorType>::is_signed, "NumeratorType must be unsigned");
        static_assert(!numeric_limits<DenominatorType>::is_signed, "DenominatorType must be unsigned");
        return (numerator + denominator - NumeratorType(1u)) / denominator;
    }

    template<typename Value, typename Modulus>
    constexpr Value makeDivisibleByRoundingDown(Value value, Modulus modulus) {
        // Doesn't work for -ve numbers.
        static_assert(!numeric_limits<Value>::is_signed, "Value must be unsigned");
        static_assert(!numeric_limits<Modulus>::is_signed, "Modulus must be unsigned");
        return (value / modulus) * modulus;
    }

    template<typename Value, typename Modulus>
    constexpr Value makeDivisibleByRoundingUp(Value value, Modulus modulus) {
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
}}}}

namespace Enhedron { namespace Util {
    using Impl::Math::divideRoundingUp;
    using Impl::Math::makeDivisibleByRoundingDown;
    using Impl::Math::makeDivisibleByRoundingUp;
    using Impl::Math::isDivisible;
}}

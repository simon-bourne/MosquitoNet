#pragma once

#include "Enhedron/Test/Suite.h"

namespace Enhedron { namespace Test {
    class Performance final: public Suite {
    public:
        template<typename... ContextListType>
        Performance(ContextListType&&... childContextList) : Suite("performance", std::forward<ContextListType>(childContextList)...) {}
    };

    class Slow final: public Suite {
    public:
        template<typename... ContextListType>
        Slow(ContextListType&&... childContextList) : Suite("slow", std::forward<ContextListType>(childContextList)...) {}
    };

    class Unit final: public Suite {
    public:
        template<typename... ContextListType>
        Unit(ContextListType&&... childContextList) : Suite("unit", std::forward<ContextListType>(childContextList)...) {}
    };

    class Integration final: public Suite {
    public:
        template<typename... ContextListType>
        Integration(ContextListType&&... childContextList) : Suite("integration", std::forward<ContextListType>(childContextList)...) {}
    };
}}

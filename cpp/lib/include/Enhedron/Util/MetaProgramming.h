// Copyright (C) 2014 Simon Bourne

#pragma once

#include <type_traits>
#include <tuple>
#include <utility>

namespace Enhedron { namespace Util { namespace Impl { namespace Impl_MetaProgramming {
    template<size_t argCount>
    struct TailArgPlaceHolder {
        static TailArgPlaceHolder instance;
    };

    template<size_t argCount>
    TailArgPlaceHolder<argCount> TailArgPlaceHolder<argCount>::instance;
}}}}

namespace std {
    template<size_t argCount>
    struct is_placeholder<::Enhedron::Util::Impl::Impl_MetaProgramming::TailArgPlaceHolder<argCount>> : integral_constant<size_t, argCount> { };
}

namespace Enhedron { namespace Util { namespace Impl { namespace Impl_MetaProgramming {
    using std::tuple;
    using std::index_sequence;
    using std::index_sequence_for;
    using std::forward;
    using std::get;
    using std::remove_reference_t;
    using std::is_array;
    using std::is_function;
    using std::enable_if_t;

    template<typename BoundFunctor, class Value, size_t... indices>
    auto bindFirst(BoundFunctor&& f, Value value, index_sequence<indices...>) {
        return bind(forward<BoundFunctor>(f), value, TailArgPlaceHolder<indices + 1>::instance...);
    }

    template<typename Functor, size_t... indices, typename... Args>
    auto extractParameterPack(Functor&& f, index_sequence<indices...>, const tuple<Args...>& args) {
        return f(get<indices>(args)...);
    }

    template<typename Functor, typename... Args>
    auto extractParameterPack(Functor&& f, const tuple<Args...>& args) {
        return extractParameterPack(forward<Functor>(f), index_sequence_for<Args...>(), args);
    }

    template<typename Functor>
    void mapParameterPack(Functor&& f) {
    }

    template<typename Functor, typename Head, typename... Args>
    void mapParameterPack(Functor&& f, const Head& head, const Args&... args) {
        f(head);
        mapParameterPack(forward<Functor>(f), args...);
    }

    template<typename Functor, typename... Args>
    void mapTuple(Functor&& f, const tuple<Args...>& args) {
        extractParameterPack(
        [f(forward<Functor>(f))] (const Args&... unpackedArgs) {
            mapParameterPack(forward<Functor>(f), unpackedArgs...);
        },
        args
        );
    }

    template<typename T, typename Enable = void>
    struct DecayArray {
        using type = T;
    };

    template<typename T>
    struct DecayArray<T, enable_if_t<is_array<remove_reference_t<T>>::value>> {
        using type = typename std::remove_extent<remove_reference_t<T>>::type*;
    };

    template<typename T>
    struct DecayArray<T, enable_if_t<is_function<remove_reference_t<T>>::value>> {
        using type = typename std::reference_wrapper<remove_reference_t<T>>::type;
    };

    template<typename T>
    using DecayArray_t = typename DecayArray<T>::type;

    template<typename... Args>
    class StoreArgs final: public NoCopy {
        tuple<remove_reference_t<Args>...> args;

        template <typename Functor, size_t... indices, typename... ExtraArgs>
        void applyExtraBeforeImpl(Functor&& functor, index_sequence<indices...>, ExtraArgs&&... extraArgs) {
            functor(forward<ExtraArgs>(extraArgs)..., forward<Args>(get<indices>(args))...);
        }

        template <typename Functor, size_t... indices, typename... ExtraArgs>
        void applyExtraAfterImpl(Functor&& functor, index_sequence<indices...>, ExtraArgs&&... extraArgs) {
            functor(forward<Args>(get<indices>(args))..., forward<ExtraArgs>(extraArgs)...);
        }
    public:
        // RValue reference arguments will be moved into this container. Everything else will be copied.
        StoreArgs(Args&&... args) : args(forward<Args>(args)...) { }

        template <typename Functor>
        void apply(Functor&& functor) {
            extractParameterPack(forward<Functor>(functor), forward<Args>(args)...);
        }

        template <typename Functor, typename... ExtraArgs>
        void applyExtraBefore(Functor&& functor, ExtraArgs&&... extraArgs) {
            applyExtraBeforeImpl(
                    forward<Functor>(functor),
                    index_sequence_for<Args...>(),
                    forward<ExtraArgs>(extraArgs)...
            );
        }

        template <typename Functor, typename... ExtraArgs>
        void applyExtraAfter(Functor&& functor, ExtraArgs&&... extraArgs) {
            applyExtraAfterImpl(
                    forward<Functor>(functor),
                    index_sequence_for<Args...>(),
                    forward<ExtraArgs>(extraArgs)...
            );
        }
    };
}}}}

namespace Enhedron { namespace Util {
    using Impl::Impl_MetaProgramming::StoreArgs;
    using Impl::Impl_MetaProgramming::DecayArray;
    using Impl::Impl_MetaProgramming::DecayArray_t;
    using Impl::Impl_MetaProgramming::bindFirst;
    using Impl::Impl_MetaProgramming::mapTuple;
    using Impl::Impl_MetaProgramming::mapParameterPack;
    using Impl::Impl_MetaProgramming::extractParameterPack;
}}

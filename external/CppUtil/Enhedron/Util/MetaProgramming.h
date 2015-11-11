// Copyright (C) 2014 Simon Bourne

#pragma once

#include <type_traits>
#include <tuple>
#include <utility>

namespace Enhedron { namespace Util { namespace Impl { namespace Impl_MetaProgramming {
    using std::tuple;
    using std::index_sequence;
    using std::index_sequence_for;
    using std::forward;
    using std::get;
    using std::remove_reference_t;

    template<typename... Args>
    class StoreArgs final: public NoCopy {
        tuple<remove_reference_t<Args>...> args;

        template <typename Functor, size_t... Indices, typename... ExtraArgs>
        void applyExtraBeforeImpl(Functor&& functor, index_sequence<Indices...>, ExtraArgs&&... extraArgs) {
            functor(forward<ExtraArgs>(extraArgs)..., forward<Args>(get<Indices>(args))...);
        }

        template <typename Functor, size_t... Indices, typename... ExtraArgs>
        void applyExtraAfterImpl(Functor&& functor, index_sequence<Indices...>, ExtraArgs&&... extraArgs) {
            functor(forward<Args>(get<Indices>(args))..., forward<ExtraArgs>(extraArgs)...);
        }
    public:
        // TODO: Think this is true. Needs unit testing.
        // RValue reference arguments will be moved into this container. Everything else will be copied.
        StoreArgs(Args&&... args) : args(forward<Args>(args)...) { }

        template <typename Functor>
        void apply(Functor&& functor) {
            applyExtraBefore(forward<Functor>(functor));
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
}}

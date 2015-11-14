// Copyright (C) 2014 Simon Bourne

#pragma once

#include <type_traits>
#include <functional>
#include <memory>

namespace Enhedron { namespace Impl { namespace Util {
    using std::enable_if;
    using std::is_base_of;
    using std::move;
    using std::make_unique;
    using std::unique_ptr;

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
        template<typename Super>
        Out(Out<Super> value) : value(value.value) { }

        explicit Out(ValueType& value) : value(&value) { }

        ValueType& get() { return *value; }
        const ValueType& get() const { return *value; }

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

    template<typename... Value>
    void unused(Value& ...) { }

    class Finally final: public NoCopy {
        // std::function requires the functor to be copyable (because it's copyable).
        struct BaseFunctor {
            virtual ~BaseFunctor() {}
            virtual void operator()() = 0;
        };

        template<typename Functor>
        class DerivedFunctor final : public BaseFunctor {
            Functor f;
        public:
            DerivedFunctor(Functor f) : f(move(f)) {}
            virtual ~DerivedFunctor() {}
            virtual void operator()() override { f(); }
        };

        unique_ptr<BaseFunctor> functor;
        bool valid = true;
    public:
        template<typename Functor>
        Finally(Functor functor) : functor(make_unique<DerivedFunctor<Functor>>(move(functor))) {}

        Finally(Finally&& source) : functor(move(source.functor)), valid(source.valid) {
            source.valid = false;
        }

        Finally& operator=(Finally&& source) {
            functor = move(source.functor);
            valid = move(source.valid);
            source.valid = false;

            return *this;
        }

        ~Finally() {
            close();
        }

        void close() {
            if (valid) {
                (*functor)();
            }

            valid = false;
        }

        template<typename Object>
        static Finally wrap(Object object) { return Finally([object(move(object))] {}); }
    };
}}}

namespace Enhedron {
    using Impl::Util::NoCopy;
    using Impl::Util::NoCopyMove;
    using Impl::Util::Out;
    using Impl::Util::out;
    using Impl::Util::unused;
    using Impl::Util::Finally;
}

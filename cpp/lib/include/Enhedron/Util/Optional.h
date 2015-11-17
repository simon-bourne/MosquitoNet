#pragma once

#include <memory>
#include <utility>
#include <type_traits>

namespace Enhedron { namespace Util { namespace Impl { namespace Impl_Optional {
    using std::unique_ptr;
    using std::make_unique;
    using std::forward;

    class None final {};

    static constexpr const None none{};
    
    template<typename Value>
    class optional final {
        unique_ptr<Value> value_;
    public:
        optional() = default;
        optional(Value&& value) : value_(make_unique<Value>(forward<Value>(value))) {}
        optional(None) {}

        Value& get() const { return *value_; }

        Value& operator*() const { return *value_; }

        Value* operator->() const { return value_.get(); }

        void reset() { value_.reset(); }

        operator bool() const { return bool(value_); }
    };

    template<typename Value>
    class optional<Value&> final {
        Value* value_ = nullptr;
    public:
        optional() = default;
        optional(Value& value) : value_(&value) {}
        optional(None) {}

        Value& get() const { return *value_; }

        Value& operator*() const { return *value_; }

        Value* operator->() const { return value_; }

        void reset() { value_ = nullptr; }

        operator bool() const { return value_ != nullptr; }
    };
}}}}

namespace Enhedron { namespace Util {
    using Impl::Impl_Optional::none;
    using Impl::Impl_Optional::optional;
}}

// This file is generated automatically. Do not edit.

#ifndef ENHEDRON_MOSQUITONET_H_
#define ENHEDRON_MOSQUITONET_H_

// File: Enhedron/Util.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//


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

        static Finally empty() { return Finally([] {}); }
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
// File: Enhedron/Util/MetaProgramming.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//


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
    using std::remove_extent_t;
    using std::conditional_t;
    using std::is_array;
    using std::is_function;
    using std::add_pointer_t;
    using std::enable_if_t;

    template<typename BoundFunctor, class Value, size_t... indices>
    auto bindFirst(BoundFunctor&& f, Value value, index_sequence<indices...>) {
        return bind(forward<BoundFunctor>(f), value, TailArgPlaceHolder<indices + 1>::instance...);
    }

    template<typename Functor, size_t... indices, typename... Args>
    auto extractParameterPack(Functor&& f, index_sequence<indices...>, const tuple<Args...>& args) {
        return f(get<indices>(args)...);
    }

    template<typename Functor, size_t... indices, typename... Args>
    auto extractParameterPack(Functor&& f, index_sequence<indices...>, tuple<Args...>&& args) {
        return f(get<indices>(forward<tuple<Args...>>(args))...);
    }

    template<typename Functor, typename... Args>
    auto extractParameterPack(Functor&& f, const tuple<Args...>& args) {
        return extractParameterPack(forward<Functor>(f), index_sequence_for<Args...>(), args);
    }

    template<typename Functor, typename... Args>
    auto extractParameterPack(Functor&& f, tuple<Args...>&& args) {
        return extractParameterPack(forward<Functor>(f), index_sequence_for<Args...>(), forward<tuple<Args...>>(args));
    }

    template<typename Functor>
    void mapParameterPack(Functor&&) {
    }

    template<typename Functor, typename Head, typename... Args>
    void mapParameterPack(Functor&& f, const Head& head, const Args&... args) {
        f(head);
        mapParameterPack(forward<Functor>(f), args...);
    }

    template<typename T>
    class DecayArrayAndFunction {
    public:
        using U = remove_reference_t<T>;
        using type = conditional_t<is_array<U>::value,
                remove_extent_t<U>*,
                conditional_t<is_function<U>::value,
                    add_pointer_t<U>,
                    T
                >>;
    };

    template<typename T>
    using DecayArrayAndFunction_t = typename DecayArrayAndFunction<T>::type;

    template<typename... Args>
    class StoreArgs final: public NoCopy {
        using TupleType = tuple<remove_reference_t<Args>...>;
        TupleType args;

        template <typename Functor, size_t... indices, typename... ExtraArgs>
        auto applyExtraBeforeImpl(Functor&& functor, index_sequence<indices...>, ExtraArgs&&... extraArgs) {
            return functor(forward<ExtraArgs>(extraArgs)..., forward<Args>(get<indices>(args))...);
        }

        template <typename Functor, size_t... indices, typename... ExtraArgs>
        auto applyExtraAfterImpl(Functor&& functor, index_sequence<indices...>, ExtraArgs&&... extraArgs) {
            return functor(forward<Args>(get<indices>(args))..., forward<ExtraArgs>(extraArgs)...);
        }
    public:
        // RValue reference arguments will be moved into this container. Everything else will be copied.
        StoreArgs(Args&&... args) : args(forward<Args>(args)...) { }

        template <typename Functor>
        auto apply(Functor&& functor) {
            return extractParameterPack(forward<Functor>(functor), args);
        }

        template <typename Functor, typename... ExtraArgs>
        auto applyExtraBefore(Functor&& functor, ExtraArgs&&... extraArgs) {
            return applyExtraBeforeImpl(
                    forward<Functor>(functor),
                    index_sequence_for<Args...>(),
                    forward<ExtraArgs>(extraArgs)...
            );
        }

        template <typename Functor, typename... ExtraArgs>
        auto applyExtraAfter(Functor&& functor, ExtraArgs&&... extraArgs) {
            return applyExtraAfterImpl(
                    forward<Functor>(functor),
                    index_sequence_for<Args...>(),
                    forward<ExtraArgs>(extraArgs)...
            );
        }
    };
}}}}

namespace Enhedron { namespace Util {
    using Impl::Impl_MetaProgramming::StoreArgs;
    using Impl::Impl_MetaProgramming::DecayArrayAndFunction;
    using Impl::Impl_MetaProgramming::DecayArrayAndFunction_t;
    using Impl::Impl_MetaProgramming::bindFirst;
    using Impl::Impl_MetaProgramming::mapParameterPack;
    using Impl::Impl_MetaProgramming::extractParameterPack;
}}
// File: Enhedron/Util/Enum.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef ENHEDRON_UTIL_ENUM_H_
#define ENHEDRON_UTIL_ENUM_H_

#include <stdexcept>
#include <type_traits>

namespace Enhedron { namespace Util { namespace Impl { namespace Enum {
    using std::runtime_error;
    using std::underlying_type_t;

    /**
     * Convert a value to an enum safely. The enum must have "LAST_ENUM_VALUE" as the last
     * enumerated value, and must be a given enum starting at 0 and being densely populated. For example:
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
// File: Enhedron/Util/Optional.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//


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

        template<typename Other>
        friend class optional;
    public:
        optional() = default;

        optional(const optional<Value>& other) {
            if (bool(other)) {
                value_ = make_unique<Value>(*other.value_);
            }
            else {
                value_.reset();
            }
        }

        optional<Value>& operator=(const optional<Value>& other) {
            if (bool(other)) {
                value_ = make_unique<Value>(*other.value_);
            }
            else {
                value_.reset();
            }

            return *this;
        }

        optional(optional<Value>&&) = default;
        optional& operator=(optional<Value>&&) = default;

        optional(const Value& value) : value_(make_unique<Value>(value)) {}
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
// File: Enhedron/Assertion/Configurable.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//



#include <type_traits>
#include <sstream>
#include <string>
#include <utility>
#include <stdexcept>
#include <functional>
#include <tuple>

#include <deque>
#include <forward_list>
#include <list>
#include <map>
#include <queue>
#include <set>
#include <stack>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace Enhedron { namespace Assertion {
    template <typename T>
    class HasOutputOperator {
        template <typename U, typename = decltype(std::declval<std::ostream&>() << std::declval<const U&>())>
        static std::integral_constant<bool, true> test();

        template <typename U, typename... Args>
        static std::integral_constant<bool, false> test(Args...);

    public:
        static constexpr bool value = decltype(test<T>())::value;
    };

    template<typename CONTAINER>
    class IsStlContainer {
        using True = std::integral_constant<bool, true>;
        template <typename U>       struct test : std::false_type {};

        template <typename Value, std::size_t N> struct test<std::array<Value, N>> : True{};

        // MSVC doesn't like the parameter packs for these. Suspect it just doesn't like the
        // parameter packs for any container and is just using the defaults for allocators.
        template <typename K, typename T, typename C, typename A>
        struct test<std::map<K, T, C, A>> : True{};

        template <typename K, typename T, typename C, typename A>
        struct test<std::multimap<K, T, C, A>> : True{};

        template <typename K, typename T, typename H, typename P, typename A>
        struct test<std::unordered_map<K, T, H, P, A>> : True{};

        template <typename K, typename T, typename H, typename P, typename A>
        struct test<std::unordered_multimap<K, T, H, P, A>> : True{};

        template <typename... Args> struct test<std::deque             <Args...>> : True{};
        template <typename... Args> struct test<std::forward_list      <Args...>> : True{};
        template <typename... Args> struct test<std::list              <Args...>> : True{};
        template <typename... Args> struct test<std::multiset          <Args...>> : True{};
        template <typename... Args> struct test<std::priority_queue    <Args...>> : True{};
        template <typename... Args> struct test<std::queue             <Args...>> : True{};
        template <typename... Args> struct test<std::set               <Args...>> : True{};
        template <typename... Args> struct test<std::stack             <Args...>> : True{};
        template <typename... Args> struct test<std::unordered_set     <Args...>> : True{};
        template <typename... Args> struct test<std::unordered_multiset<Args...>> : True{};
        template <typename... Args> struct test<std::vector            <Args...>> : True{};

    public:
        static constexpr bool value = test<std::decay_t<CONTAINER>>::value;
    };

    template<typename Value, typename Enable = void>
    struct Convert {
        template<typename T = Value, std::enable_if_t<HasOutputOperator<T>::value>* = nullptr>
        static std::string toString(const Value &value) {
            std::ostringstream valueString;
            valueString << value;

            return valueString.str();
        }

        template<typename T = Value, std::enable_if_t< ! HasOutputOperator<T>::value>* = nullptr>
        static std::string toString(const Value &value) {
            return "<unknown>";
        }
    };

    template<>
    struct Convert<std::nullptr_t> {
        static inline std::string toString(const std::nullptr_t &) {
            return "nullptr";
        }
    };

    template<>
    struct Convert<bool> {
        static inline std::string toString(bool value) {
            return value ? "true" : "false";
        }
    };

    template<>
    struct Convert<std::string> {
        static inline std::string toString(const std::string& s) {
            return "\"" + s + "\"";
        }
    };

    template<typename Value>
    struct Convert<Value, std::enable_if_t<std::is_same<std::decay_t<Value>, char*>::value>> {
        static inline std::string toString(const char* s) {
            return "\"" + std::string(s) + "\"";
        }
    };

    template<typename Value>
    struct Convert<Value, std::enable_if_t<std::is_enum<std::remove_reference_t<Value>>::value>> {
        static std::string toString(Value value) {
            std::ostringstream valueString;
            valueString << static_cast<std::underlying_type_t<std::remove_reference_t<Value>>>(value);

            return valueString.str();
        }
    };

    template<typename Value>
    struct Convert<Value, std::enable_if_t<std::is_function<std::remove_reference_t<Value>>::value>> {
        static std::string toString(Value value) {
            return "<function>";
        }
    };

    template<typename Container>
    struct Convert<Container, std::enable_if_t<IsStlContainer<Container>::value>> {
        static inline std::string toString(const Container& value) {
            std::string result("[");
            bool isFirst = true;

            for (const auto& element : value) {
                if ( ! isFirst) {
                    result += ", ";
                }

                isFirst = false;
                result += Convert<typename Container::value_type>::toString(element);
            }

            result += "]";

            return result;
        }
    };

}}

namespace Enhedron { namespace Assertion { namespace Impl { namespace Configurable {
    using Util::StoreArgs;
    using Util::mapParameterPack;
    using Util::extractParameterPack;
    using Util::DecayArrayAndFunction_t;
    using Util::optional;
    using Util::none;

    using std::enable_if_t;
    using std::is_base_of;
    using std::is_same;
    using std::ostringstream;
    using std::string;
    using std::move;
    using std::forward;
    using std::vector;
    using std::result_of_t;
    using std::exception;
    using std::function;
    using std::pair;
    using std::tuple;
    using std::tie;
    using std::add_lvalue_reference_t;
    using std::index_sequence;
    using std::index_sequence_for;
    using std::get;
    using std::reference_wrapper;
    using std::remove_reference_t;
    using std::conditional_t;
    using std::ref;
    using std::cref;
    using std::is_function;

    class Expression {
    };

    template<typename Arg>
    using IsExpression = enable_if_t<is_base_of<Expression, Arg>::value>*;

    template<typename Arg>
    using IsNotExpression = enable_if_t<!is_base_of<Expression, Arg>::value> *;

    template<typename Lhs, typename Rhs>
    using EitherIsExpression = enable_if_t<
            is_base_of<Expression, Lhs>::value || is_base_of<Expression, Rhs>::value> *;

    class Variable final {
    public:
        Variable(string name, string value, string file, int line) : name_(move(name)), value_(move(value)),
                                                                     file_(move(file)), line_(move(line)) { }

        const string &name() const { return name_; }

        const string &value() const { return value_; }

        const string &file() const { return file_; }

        int line() const { return line_; }

    private:
        string name_;
        string value_;
        string file_;
        int line_;
    };

    template<typename Value>
    class Literal final : public Expression {
    public:
        using ResultType = DecayArrayAndFunction_t<Value>;

        explicit Literal(const Value &value) : value(value) { }

        string makeName() const {
            return Convert<Value>::toString(value);
        }

        void appendVariables(vector<Variable> &) const { }

        const Value& evaluate() {
            return value;
        }

    private:
        const Value &value;
    };

    template<typename Functor, typename Arg>
    class UnaryOperator final : public Expression {
    public:
        using ResultType = DecayArrayAndFunction_t<result_of_t<Functor(typename Arg::ResultType)>>;

        explicit UnaryOperator(const char *operatorName, Functor functor, Arg arg) : operatorName(operatorName),
                                                                                     functor(move(functor)),
                                                                                     arg(move(arg)) { }

        string makeName() const {
            ostringstream valueString;
            valueString << "( " << operatorName << " " << arg.makeName() << ")";

            return valueString.str();
        }

        void appendVariables(vector<Variable> &variableList) const {
            arg.appendVariables(variableList);
        }

        ResultType evaluate() {
            return functor(arg.evaluate());
        }

    private:
        const char *operatorName;
        Functor functor;
        Arg arg;
    };

    template<typename Functor, typename Lhs, typename Rhs>
    class BinaryOperator final : public Expression {
    public:
        using ResultType = DecayArrayAndFunction_t<result_of_t<Functor(
                                typename Lhs::ResultType,
                                typename Rhs::ResultType)>>;

        explicit BinaryOperator(const char *operatorName, Functor functor, Lhs lhs, Rhs rhs) :
                operatorName(operatorName),
                functor(move(functor)),
                lhs(move(lhs)),
                rhs(move(rhs)) { }

        string makeName() const {
            ostringstream valueString;
            valueString << "(" << lhs.makeName() << " " << operatorName << " " << rhs.makeName() << ")";

            return valueString.str();
        }

        void appendVariables(vector<Variable> &variableList) const {
            lhs.appendVariables(variableList);
            rhs.appendVariables(variableList);
        }

        ResultType evaluate() {
            return functor(lhs.evaluate(), rhs.evaluate());
        }

    private:
        const char *operatorName;
        Functor functor;
        Lhs lhs;
        Rhs rhs;
    };

    template<typename T, typename Enable = void>
    struct DecayExpression {
        using type = T;
    };

    template<typename T>
    struct DecayExpression<T, enable_if_t<is_base_of<Expression, remove_reference_t<T>>::value>> {
        using type = typename T::ResultType;
    };

    template<typename T>
    using DecayExpression_t = typename DecayExpression<T>::type;

    template<typename Functor, typename... Args>
    class FunctionValue final : public Expression {
    public:
        using ResultType = DecayArrayAndFunction_t<result_of_t<Functor&(DecayExpression_t<Args>...)>>;

        explicit FunctionValue(string name, Functor&& functor, const char *file, int line, Args&&... args) :
                name(move(name)),
                functor(move(functor)),
                fileAndLine(FileLine{file, line}),
                args(forward<Args>(args)...)
        {}

        explicit FunctionValue(string name, Functor&& functor, Args&&... args) :
                name(move(name)),
                functor(move(functor)),
                args(forward<Args>(args)...)
        {}

        string makeName() const {
            ostringstream valueString;
            valueString << name << "(";
            extractParameterPack(
                    [this, &valueString] (const Args&... unpackedArgs) {
                        makeParameterNames(out(valueString), unpackedArgs...);
                    },
                    args
            );
            valueString << ")";

            if (exceptionMessage) {
                valueString << " threw \"" << *exceptionMessage << "\"";
            }

            return valueString.str();
        }

        void appendVariables(vector<Variable>& variableList) const {
            if (fileAndLine) {
                variableList.emplace_back(name, "function", fileAndLine->file, fileAndLine->line);
            }

            extractParameterPack(
                    [this, &variableList] (const Args&... unpackedArgs) {
                        appendParameterVariableList(variableList, unpackedArgs...);
                    },
                    args
            );
        }

        ResultType evaluate() {
            return extractParameterPack( [this] (Args&&... extractedArgs) {
                                             return functor(getParameterValue(forward<Args>(extractedArgs))...);
                                         },
                                         move(args)
            );
        }

        void setException(const exception& e) {
            exceptionMessage = optional<string>(e.what());
        }
    private:
        template<typename Arg1, typename Arg2, typename... Tail>
        void makeParameterNames(Out<ostringstream> parameters, const Arg1& arg1, const Arg2& arg2, const Tail&... tail) const {
            (*parameters) << getParameterName(arg1) << ", ";
            makeParameterNames(parameters, arg2, tail...);
        }

        template<typename Arg>
        void makeParameterNames(Out<ostringstream> parameters, const Arg& arg) const {
            (*parameters) << getParameterName(arg);
        }

        void makeParameterNames(Out<ostringstream> parameters) const {}

        template<typename Arg, IsExpression<Arg> = nullptr>
        string getParameterName(const Arg& arg) const {
            return arg.makeName();
        }

        template<typename Arg, IsNotExpression<Arg> = nullptr>
        string getParameterName(const Arg& arg) const {
            return Convert<Arg>::toString(arg);
        }

        template<typename Arg, IsExpression<Arg> = nullptr>
        auto getParameterValue(Arg&& arg) {
            return arg.evaluate();
        }

        template<typename Arg, IsNotExpression<Arg> = nullptr>
        auto getParameterValue(Arg&& arg) {
            return arg;
        }

        template<typename HeadArg, typename... TailArgs>
        void appendParameterVariableList(
                vector<Variable>& variableList,
                const HeadArg& arg,
                const TailArgs&... tailArgs
            ) const
        {
            appendParameterVariable(variableList, arg);
            appendParameterVariableList(variableList, tailArgs...);
        }

        void appendParameterVariableList(vector<Variable>& variableList) const {}

        template<typename Arg, IsExpression<Arg> = nullptr>
        void appendParameterVariable(vector<Variable>& variableList, const Arg& arg) const {
            return arg.appendVariables(variableList);
        }

        template<typename Arg, IsNotExpression<Arg> = nullptr>
        void appendParameterVariable(vector<Variable>&, const Arg&) const {
        }

        string name;
        Functor functor;

        struct FileLine {
            const char* file;
            int line;
        };

        optional<FileLine> fileAndLine;
        optional<string> exceptionMessage;
        tuple<Args...> args;
    };

    template<typename Functor>
    class Function final {
        string name_;
        Functor functor_;
    public:
        Function(string name, Functor&& functor) : name_(move(name)), functor_(functor) {}

        template<typename... Args>
        auto operator()(Args&&... args) {
            return FunctionValue<Functor, Args...>(name_, Functor(functor_), forward<Args>(args)...);
        }
    };

    template<typename Functor>
    auto makeFunction(string name, Functor&& functor) {
        return Function<Functor>(move(name), forward<Functor>(functor));
    }

    template<typename Value>
    class VariableRefExpression final : public Expression {
    public:
        using ResultType = DecayArrayAndFunction_t<Value>;

        explicit VariableRefExpression(const char *variableName, Value& value, const char *file, int line) :
                variableName(variableName),
                value(value),
                file(file),
                line(line) { }

        string makeName() const {
            return variableName;
        }

        void appendVariables(vector<Variable> &variableList) const {
            variableList.emplace_back(variableName, Convert<Value>::toString(value), file, line);
        }

        ResultType evaluate() {
            return value;
        }

        template<typename... Args>
        auto operator()(Args&&... args) {
            return FunctionValue<reference_wrapper<Value>, Args...>(
                    variableName, cref(value), file, line, forward<Args>(args)...
                );
        }

    private:
        const char *variableName;
        Value& value;
        const char *file;
        int line;
    };

    template<typename Value>
    class VariableValueExpression final : public Expression {
    public:
        using ResultType = DecayArrayAndFunction_t<Value>;

        explicit VariableValueExpression(const char *variableName, Value&& value, const char *file, int line) :
                variableName(variableName),
                value(move(value)),
                file(file),
                line(line) { }

        string makeName() const {
            return variableName;
        }

        void appendVariables(vector<Variable> &variableList) const {
            variableList.emplace_back(variableName, Convert<Value>::toString(value), file, line);
        }

        ResultType evaluate() {
            return value;
        }

        // Only call this once! It will move the underlying value.
        template<typename... Args>
        FunctionValue<Value, Args...> operator()(Args&&... args) {
            return FunctionValue<Value, Args...>(variableName, move(value), file, line, forward<Args>(args)...);
        }

    private:
        const char *variableName;
        Value value;
        const char *file;
        int line;
    };

    template<typename Value>
    auto makeVariable(const char *name, Value& value, const char *file, int line) {
        return VariableRefExpression<Value>(name, value, file, line);
    }

    template<typename Value, enable_if_t<is_function<Value>::value>* = nullptr>
    auto makeVariable(const char *name, Value&& value, const char *file, int line) {
        return VariableValueExpression<reference_wrapper<Value>>(name, ref(value), file, line);
    }

    template<typename Value, enable_if_t< ! is_function<Value>::value>* = nullptr>
    auto makeVariable(const char *name, Value&& value, const char *file, int line) {
        return VariableValueExpression<Value>(name, move(value), file, line);
    }

    template<typename Operation, typename Lhs, typename Rhs, pair<IsExpression<Lhs>, IsExpression<Rhs>> * = nullptr>
    auto apply(const char *operationName, Operation operation, const Lhs &lhs, const Rhs &rhs) {
        return BinaryOperator<Operation, Lhs, Rhs>(operationName, move(operation), lhs, rhs);
    }

    template<typename Operation, typename Lhs, typename Rhs, pair<IsExpression<Lhs>, IsNotExpression<Rhs>> * = nullptr>
    auto apply(const char *operationName, Operation operation, const Lhs &lhs, const Rhs &rhs) {
        return apply(operationName, move(operation), lhs, Literal<Rhs>(rhs));
    }

    template<typename Operation, typename Lhs, typename Rhs, pair<IsNotExpression<Lhs>, IsExpression<Rhs>> * = nullptr>
    auto apply(const char *operationName, Operation operation, const Lhs &lhs, const Rhs &rhs) {
        return apply(operationName, move(operation), Literal<Lhs>(lhs), rhs);
    }

    template<typename Operation, typename Arg, IsExpression<Arg> = nullptr>
    auto apply(const char *operationName, Operation operation, const Arg &arg) {
        return UnaryOperator<Operation, Arg>(operationName, move(operation), arg);
    }

    // Unary operators
    template<typename Arg, IsExpression<Arg> = nullptr>
    auto operator!(const Arg &arg) {
        return apply("!", [](const auto& arg) { return !arg; }, arg);
    }

    template<typename Arg, IsExpression<Arg> = nullptr>
    auto operator~(const Arg &arg) {
        return apply("~", [](const auto& arg) { return ~arg; }, arg);
    }

    template<typename Arg, IsExpression<Arg> = nullptr>
    auto operator+(const Arg &arg) {
        return apply("+", [](const auto& arg) { return +arg; }, arg);
    }

    template<typename Arg, IsExpression<Arg> = nullptr>
    auto operator-(const Arg &arg) {
        return apply("-", [](const auto& arg) { return -arg; }, arg);
    }


    // Binary operators
    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator==(const Lhs &lhs, const Rhs &rhs) {
        return apply("==", [](const auto&lhs, const auto& rhs) { return lhs == rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator!=(const Lhs &lhs, const Rhs &rhs) {
        return apply("!=", [](const auto&lhs, const auto& rhs) { return lhs != rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator>(const Lhs &lhs, const Rhs &rhs) {
        return apply(">", [](const auto&lhs, const auto& rhs) { return lhs > rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator>=(const Lhs &lhs, const Rhs &rhs) {
        return apply(">=", [](const auto&lhs, const auto& rhs) { return lhs >= rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator<(const Lhs &lhs, const Rhs &rhs) {
        return apply("<", [](const auto&lhs, const auto& rhs) { return lhs < rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator<=(const Lhs &lhs, const Rhs &rhs) {
        return apply("<=", [](const auto&lhs, const auto& rhs) { return lhs <= rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator&&(const Lhs &lhs, const Rhs &rhs) {
        return apply("&&", [](const auto&lhs, const auto& rhs) { return lhs && rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator||(const Lhs &lhs, const Rhs &rhs) {
        return apply("||", [](const auto&lhs, const auto& rhs) { return lhs || rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator+(const Lhs &lhs, const Rhs &rhs) {
        return apply("+", [](const auto&lhs, const auto& rhs) { return lhs + rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator-(const Lhs &lhs, const Rhs &rhs) {
        return apply("-", [](const auto&lhs, const auto& rhs) { return lhs - rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator*(const Lhs &lhs, const Rhs &rhs) {
        return apply("*", [](const auto&lhs, const auto& rhs) { return lhs * rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator/(const Lhs &lhs, const Rhs &rhs) {
        return apply("/", [](const auto&lhs, const auto& rhs) { return lhs / rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator%(const Lhs &lhs, const Rhs &rhs) {
        return apply("%", [](const auto&lhs, const auto& rhs) { return lhs % rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator&(const Lhs &lhs, const Rhs &rhs) {
        return apply("&", [](const auto&lhs, const auto& rhs) { return lhs & rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator|(const Lhs &lhs, const Rhs &rhs) {
        return apply("|", [](const auto&lhs, const auto& rhs) { return lhs | rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator^(const Lhs &lhs, const Rhs &rhs) {
        return apply("^", [](const auto&lhs, const auto& rhs) { return lhs ^ rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator<<(const Lhs &lhs, const Rhs &rhs) {
        return apply("<<", [](const auto&lhs, const auto& rhs) { return lhs << rhs; }, lhs, rhs);
    }

    template<typename Lhs, typename Rhs, EitherIsExpression<Lhs, Rhs> = nullptr>
    auto operator>>(const Lhs &lhs, const Rhs &rhs) {
        return apply(">>", [](const auto&lhs, const auto& rhs) { return lhs >> rhs; }, lhs, rhs);
    }

    struct FailureHandler {
        virtual ~FailureHandler() {};
        virtual bool notifyPassing() const = 0;
        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) = 0;
        virtual void fail(optional<string> description, const string &expressionText, const vector <Variable> &variableList) = 0;
    };

    template<typename Expression, typename... ContextVariableList>
    vector<Variable> buildVariableList(
            const Expression& expression,
            const ContextVariableList&... contextVariableList
    ) {
        vector<Variable> variableList;
        expression.appendVariables(variableList);
        vector<Variable> contextVariableVector(contextVariableList...);
        variableList.insert(variableList.end(), contextVariableVector.begin(), contextVariableVector.end());

        return variableList;
    }

    template<typename Expression, typename... ContextVariableList>
    void processFailure(
            Out<FailureHandler> failureHandler,
            optional<string> description,
            Expression expression,
            ContextVariableList... contextVariableList
    ) {
        failureHandler->fail(move(description), expression.makeName(), buildVariableList(expression, contextVariableList...));
    }

    template<typename Expression, typename... ContextVariableList>
    void processSuccess(
            Out<FailureHandler> failureHandler,
            optional<string> description,
            Expression expression,
            ContextVariableList... contextVariableList
    ) {
        if (failureHandler->notifyPassing()) {
            failureHandler->pass(move(description), expression.makeName(), buildVariableList(expression, contextVariableList...));
        }
    }

    template<typename Expression, typename... Tail>
    bool CheckWithFailureHandlerImpl(
            Out<FailureHandler> failureHandler,
            optional<string> description,
            Expression expression,
            Tail... tail
    ) {
        if ( ! static_cast<bool>(expression.evaluate())) {
            processFailure(failureHandler, move(description), move(expression), move(tail)...);

            return false;
        }

        processSuccess(failureHandler, move(description), move(expression), move(tail)...);

        return true;
    }

    template<typename Expression, typename... Tail, IsExpression<Expression> = nullptr>
    bool CheckWithFailureHandler(
            Out<FailureHandler> failureHandler,
            Expression expression,
            Tail... tail
    ) {
        return CheckWithFailureHandlerImpl(failureHandler, none, move(expression), move(tail)...);
    }

    template<typename... Tail, IsExpression<Expression> = nullptr>
    bool CheckWithFailureHandler(
            Out<FailureHandler> failureHandler,
            string description,
            Tail... tail
    ) {
        return CheckWithFailureHandlerImpl(failureHandler, move(description), move(tail)...);
    }

    template<
            typename Exception,
            typename Expression,
            typename... ContextVariableList,
            enable_if_t<is_same<Exception, exception>::value> * = nullptr
    >
    bool CheckThrowsWithFailureHandlerImpl(
            Out<FailureHandler> failureHandler,
            optional<string> description,
            Expression expression,
            ContextVariableList... contextVariableList
    ) {
        try {
            expression.evaluate();
            processFailure(failureHandler, move(description), move(expression), move(contextVariableList)...);

            return false;
        }
        catch (const exception&) {
            processSuccess(failureHandler, move(description), move(expression), move(contextVariableList)...);
        }

        return true;
    }

    template<
            typename Exception,
            typename Functor,
            typename... Args,
            typename... ContextVariableList,
            enable_if_t<!is_same<Exception, exception>::value> * = nullptr
    >
    bool CheckThrowsWithFailureHandlerImpl(
            Out<FailureHandler> failureHandler,
            optional<string> description,
            FunctionValue<Functor, Args...> expression,
            ContextVariableList... contextVariableList
    ) {
        try {
            expression.evaluate();
            processFailure(failureHandler, move(description), move(expression), move(contextVariableList)...);

            return false;
        }
        catch (const Exception&) {
            processSuccess(failureHandler, move(description), move(expression), move(contextVariableList)...);
        }
        catch (const exception &e) {
            expression.setException(e);
            processFailure(failureHandler, move(description), move(expression), move(contextVariableList)...);

            return false;
        }

        return true;
    }

    template<
            typename Exception,
            typename Expression,
            typename... Tail,
            IsExpression<Expression> = nullptr
    >
    bool CheckThrowsWithFailureHandler(
            Out<FailureHandler> failureHandler,
            Expression expression,
            Tail... tail
    ) {
        return CheckThrowsWithFailureHandlerImpl<Exception>(failureHandler, none, move(expression), move(tail)...);
    }

    template<
            typename Exception,
            typename... Tail
    >
    bool CheckThrowsWithFailureHandler(
            Out<FailureHandler> failureHandler,
            string description,
            Tail... tail
    ) {
        return CheckThrowsWithFailureHandlerImpl<Exception>(failureHandler, move(description), move(tail)...);
    }
}}}}

namespace Enhedron { namespace Assertion {
    using Impl::Configurable::CheckWithFailureHandler;
    using Impl::Configurable::CheckThrowsWithFailureHandler;
    using Impl::Configurable::IsExpression;
    using Impl::Configurable::Expression;
    using Impl::Configurable::Variable;
    using Impl::Configurable::makeFunction;
    using Impl::Configurable::processFailure;
    using Impl::Configurable::FailureHandler;
    using Impl::Configurable::Function;
}}

#define M_ENHEDRON_VAL(expression) \
    (::Enhedron::Assertion::Impl::Configurable::makeVariable((#expression), (expression), (__FILE__), (__LINE__)))
// File: Enhedron/Assertion.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//



#include <vector>
#include <string>
#include <iostream>
#include <utility>

namespace Enhedron { namespace Impl { namespace Impl_Assertion {
    using ::Enhedron::Util::optional;

    using std::forward;
    using std::terminate;
    using std::vector;
    using std::string;
    using std::cerr;

    using namespace ::Enhedron::Assertion;

    struct CerrFailureHandler final: FailureHandler {
        virtual ~CerrFailureHandler() override {}

        virtual bool notifyPassing() const override { return false; }

        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override { }

        virtual void fail(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            cerr << "Assert failed: " << expressionText << "\n";

            for (const auto &variable : variableList) {
                cerr << "    " << variable.name() << " = " << variable.value() <<
                ": in file " << variable.file() << ", line " << variable.line() << "\n";
            }

            cerr.flush();

            #ifndef NDEBUG
                terminate();
            #endif
        }
    };

    static CerrFailureHandler failureHandler;

    template<typename... Args>
    void Assert(Args&&... args) {
        CheckWithFailureHandler(out(failureHandler), forward<Args>(args)...);
    }

    template<typename Exception, typename... Args>
    void AssertThrows(Args&&... args) {
        CheckThrowsWithFailureHandler<Exception>(out(failureHandler), forward<Args>(args)...);
    }
}}}

namespace Enhedron {
    using Impl::Impl_Assertion::Assert;
}

#define VAR M_ENHEDRON_VAL
// File: Enhedron/Test/Results.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//



#include <memory>
#include <string>
#include <stdexcept>
#include <vector>
#include <algorithm>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Results {
    using std::exception;
    using std::string;
    using std::unique_ptr;
    using std::make_unique;
    using std::ostream;
    using std::endl;
    using std::vector;
    using std::move;
    using std::min;
    using std::max;

    using Assertion::FailureHandler;
    using Assertion::Variable;
    using Util::optional;

    class Stats {
        uint64_t fixtures_ = 0;
        uint64_t tests_ = 0;
        uint64_t checks_ = 0;
        uint64_t failedTests_ = 0;
        uint64_t failedChecks_ = 0;
    public:
        Stats& operator+=(Stats rhs) {
            fixtures_ += rhs.fixtures_;
            tests_ += rhs.tests_;
            checks_ += rhs.checks_;
            failedTests_ += rhs.failedTests_;
            failedChecks_ += rhs.failedChecks_;
            return *this;
        }

        void addFixture() { ++fixtures_; }
        void addTest() { ++tests_; }
        void addCheck() { ++checks_; }

        void failTest() { ++failedTests_; }
        void failCheck() { ++failedChecks_; }

        uint64_t fixtures() const { return fixtures_; }
        uint64_t tests() const { return tests_; }
        uint64_t checks() const { return checks_; }
        uint64_t failedTests() const { return failedTests_; }
        uint64_t failedChecks() const { return failedChecks_; }
    };

    // Wrapper, as we may add more functionality here. Such as tracking how much of the stack has
    // been seen before.
    class NameStack final: public NoCopy {
        vector<string> stack_;
    public:
        const vector<string>& stack() const { return stack_; }

        void push(string name) {
            stack_.emplace_back(move(name));
        }

        void pop() {
            Assert( ! VAR(stack_.empty()));
            stack_.pop_back();
        }
    };

    struct Results: public NoCopy {
        virtual ~Results() {}

        virtual void finish(const Stats& stats) = 0;

        virtual void beginContext(const NameStack& contextStack, const string& name) = 0;
        virtual void endContext(const Stats& stats, const NameStack& contextStack, const string& name) = 0;

        virtual void beginGiven(const NameStack& context, const string& given) = 0;
        virtual void endGiven(const Stats& stats, const NameStack& context, const string& given) = 0;

        virtual void beginWhen(const NameStack& context,
                               const string& given,
                               const NameStack& whenStack,
                               const string& when) = 0;
        virtual void endWhen(const Stats& stats,
                             const NameStack& context,
                             const string& given,
                             const NameStack& whenStack,
                             const string& when) = 0;

        virtual bool notifyPassing() const = 0;

        virtual void fail(const NameStack& context,
                          const string& given,
                          const NameStack& whenStack,
                          optional<string> description,
                          const string &expressionText,
                          const vector <Variable> &variableList) = 0;

        virtual void pass(const NameStack& context,
                          const string& given,
                          const NameStack& whenStack,
                          optional <string> description,
                          const string &expressionText,
                          const vector <Variable> &variableList) = 0;

        virtual void failByException(const NameStack& context,
                                     const string& given,
                                     const NameStack& whenStack,
                                     const exception& e) = 0;
    };

    enum class Verbosity {
        SILENT,
        SUMMARY,
        CONTEXTS,
        FIXTURES,
        SECTIONS,
        EXHAUSTIVE_SECTIONS,
        CHECKS,
        CHECKS_EXPRESSION,
        VARIABLES
    };

    enum class WrittenState {
        NONE,
        CONTEXT,
        GIVEN
    };

    class HumanResults final: public Results {
        Out<ostream> output_;
        Verbosity verbosity_;
        WrittenState writtenState_ = WrittenState::NONE;
        size_t whenDepth_ = 0;
        size_t whenWrittenDepth_ = 0;

        void setMaxWrittenState(WrittenState maxState) {
            if (writtenState_ > maxState) writtenState_ = maxState;
        }

        bool writeNeeded(WrittenState state) {
            if (writtenState_ < state) {
                writtenState_ = state;
                return true;
            }

            return false;
        }

        void writeContext(const NameStack& contextStack) {
            if (writeNeeded(WrittenState::CONTEXT)) {
                if ( ! contextStack.stack().empty()) {
                    *output_ << contextStack.stack().front();

                    for (
                            auto contextIter = contextStack.stack().begin() + 1;
                            contextIter != contextStack.stack().end();
                            ++contextIter
                        )
                    {
                        *output_ << "/" << *contextIter;
                    }

                    *output_ << "\n";
                }
            }
        }

        void writeGiven(const NameStack& context, const string& given) {
            writeContext(context);

            if (writeNeeded(WrittenState::GIVEN)) {
                indent(1);
                *output_ << "Given: " << given << "\n";
            }
        }

        void writeWhenStack(const NameStack& context, const string& given, const NameStack& when) {
            writeGiven(context, given);

            size_t depth = whenWrittenDepth_;
            auto startIndex = static_cast<vector<string>::difference_type>(whenWrittenDepth_);

            for (
                    auto whenIter = when.stack().begin() + startIndex;
                    whenIter != when.stack().end();
                    ++whenIter
                )
            {
                ++depth;
                indent(depth);
                *output_ << " When: " << *whenIter << "\n";
            }

            whenWrittenDepth_ = when.stack().size();
        }

        void printVariables(const vector <Variable> &variableList) {
            for (const auto& variable : variableList) {
                indent(whenDepth() + 1);
                (*output_) << variable.name() << " = " << variable.value()
                           << ": file \"" << variable.file() << "\", line " << variable.line() << ".\n";
            }
        }

        void indent(size_t indent) {
            while (indent > 0) {
                *output_ << "    ";
                --indent;
            }
        }

        size_t whenDepth() const {
            static constexpr const size_t minDepth = 1;

            return max(minDepth, whenDepth_);
        }
    public:
        HumanResults(Out<ostream> output, Verbosity verbosity) :
            output_(output), verbosity_(verbosity)
        {}

        virtual void finish(const Stats& stats) override {
            if (verbosity_ >= Verbosity::SUMMARY) {
                if (stats.failedTests() > 0) {
                    *output_ << "FAILED TESTS: " << stats.failedTests() << "\n";
                }

                if (stats.failedChecks() > 0) {
                    *output_ << "FAILED CHECKS: " << stats.failedChecks() << "\n";
                }

                *output_ << "Totals: " <<
                stats.tests() << " tests, " <<
                stats.checks() << " checks, " <<
                stats.fixtures() << " fixtures\n";
            }
        }

        virtual void beginContext(const NameStack& contextStack, const string& name) override {
            setMaxWrittenState(WrittenState::NONE);
        }

        virtual void endContext(const Stats& stats, const NameStack& context, const string& name) override {
            setMaxWrittenState(WrittenState::NONE);
        }

        virtual void beginGiven(const NameStack& context, const string& given) override {
            setMaxWrittenState(WrittenState::GIVEN);

            if (verbosity_ >= Verbosity::CONTEXTS) {
                writeContext(context);
            }

            if (verbosity_ >= Verbosity::FIXTURES) {
                writeGiven(context, given);
            }
        }

        virtual void endGiven(const Stats& stats, const NameStack& context, const string& given) override {
            setMaxWrittenState(WrittenState::CONTEXT);
        }

        virtual void beginWhen(const NameStack& context,
                               const string& given,
                               const NameStack& whenStack,
                               const string& when) override {
            ++whenDepth_;

            if (verbosity_ >= Verbosity::SECTIONS) {
                indent(whenDepth());
                *output_ << " When: " << when << "\n";
                whenWrittenDepth_ = whenDepth_;
            }
        }

        virtual void endWhen(const Stats& stats,
                             const NameStack& context,
                             const string& given,
                             const NameStack& whenStack,
                             const string& when) override {
            --whenDepth_;
            whenWrittenDepth_ = min(whenDepth_, whenWrittenDepth_);

            if (whenDepth_ == 0 && verbosity_ >= Verbosity::CHECKS) {
                *output_ << "\n";
            }

            setMaxWrittenState(WrittenState::GIVEN);
        }

        virtual bool notifyPassing() const override { return verbosity_ >= Verbosity::CHECKS; }

        virtual void fail(const NameStack& context,
                          const string& given,
                          const NameStack& whenStack,
                          optional<string> description,
                          const string &expressionText,
                          const vector <Variable> &variableList) override
        {
            writeWhenStack(context, given, whenStack);
            indent(whenDepth());
            (*output_) << "FAILED: Then";

            if (description) {
                *output_ << ": " << *description;
            }

            *output_ << ": " << expressionText << "\n";
            printVariables(variableList);
        }

        virtual void pass(const NameStack& context,
                          const string& given,
                          const NameStack& whenStack,
                          optional <string> description,
                          const string &expressionText,
                          const vector <Variable> &variableList) override
        {
            indent(whenDepth());
            (*output_) << " Then";

            if (description) {
                (*output_) << ": " << *description;
            }

            if (verbosity_ >= Verbosity::CHECKS_EXPRESSION || ! description) {
                (*output_) << ": " << expressionText;
            }

            (*output_) << "\n";

            if (verbosity_ >= Verbosity::VARIABLES) {
                printVariables(variableList);
            }
        }

        virtual void failByException(const NameStack& context,
                                     const string& given,
                                     const NameStack& whenStack,
                                     const exception& e) override {
            indent(whenDepth());
            (*output_) << "TEST FAILED WITH EXCEPTION: " << e.what() << endl;
        }
    };
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Results::NameStack;
    using Impl::Impl_Results::Results;
    using Impl::Impl_Results::HumanResults;
    using Impl::Impl_Results::Stats;
    using Impl::Impl_Results::Verbosity;
}}
// File: Enhedron/Test/Suite.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//




#include <functional>
#include <algorithm>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <stdexcept>
#include <tuple>
#include <iostream>
#include <regex>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Suite {
    using namespace Assertion;

    using Util::TaggedValue;
    using Util::StoreArgs;
    using Util::DecayArrayAndFunction_t;
    using Util::bindFirst;

    using Util::optional;
    using Util::none;

    using std::forward;
    using std::vector;
    using std::move;
    using std::exception;
    using std::function;
    using std::string;
    using std::unique_ptr;
    using std::shared_ptr;
    using std::make_unique;
    using std::tuple;
    using std::index_sequence;
    using std::index_sequence_for;
    using std::get;
    using std::remove_reference;
    using std::bind;
    using std::ref;
    using std::runtime_error;
    using std::cout;
    using std::regex;
    using std::regex_match;
    using std::reference_wrapper;
    using std::min;

    using PathList = vector<shared_ptr<vector<regex>>>;

    class ContextResultsRecorder final : public NoCopy {
        Out<Results> results_;
        NameStack contextStack_;
    public:
        ContextResultsRecorder(Out<Results> results) : results_(results) {
        }

        const NameStack& contextStack() const { return contextStack_; }

        void push(string name) {
            results_->beginContext(contextStack_, name);
            contextStack_.push(move(name));
        }

        void pop(const Stats& stats) {
            auto name = contextStack_.stack().back();
            results_->endContext(stats, contextStack_, name);
            contextStack_.pop();
        }

        Out<Results> beginGiven(const string& name) { results_->beginGiven(contextStack_, name); return results_; }

        void endGiven(const Stats& stats, const string& name) { results_->endGiven(stats, contextStack_, name); }
    };

    class WhenResultRecorder final: public FailureHandler {
        Out<Results> results_;
        const NameStack& contextStack_;
        string given_;
        NameStack whenStack_;
    public:
        WhenResultRecorder(Out<Results> results, const NameStack& contextStack, string given) :
                results_(results), contextStack_(contextStack), given_(move(given)) {}

        void push(string name) {
            results_->beginWhen(contextStack_, given_, whenStack_, name);
            whenStack_.push(move(name));
        }

        void pop(const Stats& stats) {
            string name = whenStack_.stack().back();
            whenStack_.pop();
            results_->endWhen(stats, contextStack_, given_, whenStack_, name);
        }

        void failByException(const exception& e) {
            results_->failByException(contextStack_, given_, whenStack_, e);
        }

        virtual bool notifyPassing() const override { return results_->notifyPassing(); }

        virtual void fail(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            return results_->fail(contextStack_, given_, whenStack_, description, expressionText, variableList);
        }

        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            return results_->pass(contextStack_, given_, whenStack_, description, expressionText, variableList);
        }

    };

    class Context: public NoCopy {
    public:
        virtual ~Context() {}

        virtual void list(const PathList& pathList, Out<ContextResultsRecorder> results, size_t depth) const = 0;
        virtual Stats run(const PathList& pathList, Out<ContextResultsRecorder> resultStack, size_t depth) = 0;
    };

    using ContextList = vector<unique_ptr<Context>>;

    class Register final: public NoCopyMove {
    public:
        static void add(unique_ptr<Context> context) {
            instance().contextList.emplace_back(move(context));
        }

        static void list(const PathList& pathList, Out<Results> results) {
            ContextResultsRecorder resultsRecorder(results);

            for (const auto& context : instance().contextList) {
                context->list(pathList, out(resultsRecorder), 0);
            }
        }

        static Stats run(const PathList& pathList, Out<Results> results) {
            Stats stats;

            ContextResultsRecorder resultsRecorder(results);

            for (const auto& context : instance().contextList) {
                stats += context->run(pathList, out(resultsRecorder), 0);
            }

            results->finish(stats);
            return stats;
        }
    private:
        Register() = default;

        static Register& instance() {
            static Register theInstance;

            return theInstance;
        }

        ContextList contextList;
    };

    class NodeContext final: public Context {
    public:
        NodeContext(const string& name, vector<unique_ptr<Context>> contextList) :
            name(name),
            contextList(move(contextList))
        {}

        virtual void list(const PathList& pathList, Out<ContextResultsRecorder> results, size_t depth) const override {
            auto nextPathList = getMatchingPaths(pathList, depth);
            results->push(name);

            if (nextPathList) {
                for (const auto& context : contextList) {
                    context->list(*nextPathList, results, depth + 1);
                }
            }

            Stats stats;
            results->pop(stats); // TODO
        }

        virtual Stats run(const PathList& pathList, Out<ContextResultsRecorder> results, size_t depth) override {
            Stats stats;
            auto nextPathList = getMatchingPaths(pathList, depth);
            results->push(name);

            if (nextPathList) {
                for (const auto& context : contextList) {
                    stats += context->run(*nextPathList, results, depth + 1);
                }
            }

            results->pop(stats);

            return stats;
        }
    private:
        optional<PathList> getMatchingPaths(const PathList& pathList, size_t depth) const {
            if (pathList.empty()) {
                return pathList;
            }

            vector<shared_ptr<vector<regex>>> nextPathList;

            for (const auto& path : pathList) {
                if (path->size() > depth) {
                    if (regex_match(name, (*path)[depth])) {
                        nextPathList.push_back(path);
                    }
                }
                else {
                    return pathList;
                }
            }

            if (nextPathList.empty()) {
                return none;
            }

            return move(nextPathList);
        }

        string name;
        vector<unique_ptr<Context>> contextList;
    };

    template<typename... ContextListType>
    void makeContextList(Out<ContextList> contextList, unique_ptr<Context> childContext, ContextListType&&... childContextList) {
        contextList->emplace_back(move(childContext));
        makeContextList(contextList, forward<ContextListType>(childContextList)...);
    }

    inline void makeContextList(Out<ContextList>) {
    }

    template<typename... ContextListType>
    unique_ptr<Context> context(const string& name, ContextListType&&... childContextList) {
        ContextList contextList;
        makeContextList(out(contextList), forward<ContextListType>(childContextList)...);

        return make_unique<NodeContext>(name, move(contextList));
    }

    class Suite: public NoCopy {
    public:
        template<typename... ContextListType>
        Suite(const string& name, ContextListType&&... childContextList) {
            Register::add(context(name, forward<ContextListType>(childContextList)...));
        }
    };

    class WhenRunner final: public FailureHandler {
        struct StackElement {
            size_t index = 0;
            size_t current = 0;
        };

        Stats stats_;
        vector<StackElement> whenStack;
        WhenResultRecorder whenResultRecorder_;
        size_t whenDepth_ = 0;

        bool topWhenDone() const {
            const auto& top = whenStack.back();

            return top.current == top.index + 1;
        }
    public:
        WhenRunner(Out<Results> results, const NameStack& contextStack, string given) :
                whenResultRecorder_(results, contextStack, move(given)) {}

        template<typename Functor, typename... Args>
        void run(Functor&& functor, Args&&... args);

        template<typename Functor>
        void when(string description, Functor&& functor) {
            if (whenStack.size() <= whenDepth_) {
                whenStack.push_back(StackElement{});
            }

            Finally stack([&] { ++whenStack[whenDepth_].current; });

            if (whenStack[whenDepth_].index == whenStack[whenDepth_].current) {
                ++whenDepth_;
                whenResultRecorder_.push(move(description));

                Finally depth([&] {
                    --whenDepth_;
                    Stats stats; // TODO
                    whenResultRecorder_.pop(stats);
                });

                functor();
            }
        }

        virtual bool notifyPassing() const override { return whenResultRecorder_.notifyPassing(); }

        virtual void fail(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            return whenResultRecorder_.fail(description, expressionText, variableList);
        }

        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            return whenResultRecorder_.pass(description, expressionText, variableList);
        }

        const Stats& stats() const { return stats_; }
    };

    class Check : public NoCopy {
        Out<WhenRunner> whenRunner_;
        Stats stats_;

        friend inline Stats checkStats(const Check& check) {
            return check.stats_;
        }

        bool addCheck(bool ok) {
            stats_.addCheck();

            if ( ! ok) {
                stats_.failCheck();
            }

            return ok;
        }
    public:
        Check(Out<WhenRunner> whenRunner) : whenRunner_(whenRunner) {}

        template<typename Functor>
        void when(string description, Functor&& functor) {
            whenRunner_->when(move(description), forward<Functor>(functor));
        }

        template<typename... Args>
        bool operator()(Args&&... args) {
            // TODO: Wrap with failure handler class that has when stack and context stack.
            return addCheck(CheckWithFailureHandler(
                    whenRunner_,
                    forward<Args>(args)...
            ));
        }

        template<typename Exception = exception, typename... Args>
        bool throws(Args&&... args) {
            return addCheck(CheckThrowsWithFailureHandler<Exception>(
                    whenRunner_,
                    forward<Args>(args)...
            ));
        }

        template<typename Expression, typename... ContextVariableList>
        void fail(Expression expression, ContextVariableList... contextVariableList) {
            processFailure(whenRunner_, none, move(expression), move(contextVariableList)...);
            stats_.failCheck();
        }
    };

    template<typename Functor, typename... Args>
    void WhenRunner::run(Functor&& functor, Args&&... args) {
        whenStack.clear();
        whenDepth_ = 0;
        stats_.addFixture(); // TODO:

        do {
            for (auto& element : whenStack) {
                element.current = 0;
            }

            Check check(out(*this));

            try {
                functor(check, forward<Args>(args)...);
            }
            catch (const exception& e) {
                whenResultRecorder_.failByException(e);
                stats_.failTest();
            }

            stats_.addTest(); // TODO

            while ( ! whenStack.empty() && topWhenDone()) {
                whenStack.pop_back();
            }

            if ( ! whenStack.empty()) {
                ++whenStack.back().index;
            }
        } while ( ! whenStack.empty());

        if (stats_.tests() == 0) {
            stats_.addTest();
        }
    }


    template<typename Functor, typename... Args>
    class Runner final: public Context {
        bool included(const PathList& pathList, size_t depth) const {
            if (pathList.empty()) {
                return true;
            }

            for (const auto& path : pathList) {
                if (path->size() > depth) {
                    if (regex_match(name, (*path)[depth])) {
                        return true;
                    }
                }
                else {
                    return true;
                }
            }

            return false;
        }

        string name;
        Functor runTest;
        StoreArgs<Args...> args;
    public:
        Runner(string name, Functor runTest, Args&&... args) :
            name(move(name)), runTest(move(runTest)), args(forward<Args>(args)...)
        {}

        virtual void list(const PathList& pathList, Out<ContextResultsRecorder> results, size_t depth) const override {
            if ( ! included(pathList, depth)) return;

            results->beginGiven(name);
            Stats stats;
            results->endGiven(stats, name);
        }

        // Must only be called once as it forwards the constructor arguments to the class.
        virtual Stats run(const PathList& pathList, Out<ContextResultsRecorder> results, size_t depth) override {
            Stats stats;

            if (included(pathList, depth)) {
                stats += args.applyExtraBefore(runTest, name, results);
            }

            return stats;
        }
    };

    template<typename Functor, typename... Args>
    class RunTest final: public NoCopy {
        Functor runTest;
    public:
        RunTest(Functor runTest) : runTest(move(runTest)) {}

        // Visual C++ 2015 seems to generate a dodgy move constructor here.
        RunTest(RunTest&& other) : runTest(move(other.runTest)) {}

        RunTest operator=(RunTest&& other) {
            runTest = move(other.runTest);
        }

        Stats operator()(const string& name, Out<ContextResultsRecorder> results, Args&&... args) {
            auto test = results->beginGiven(name);
            WhenRunner whenRunner(out(*test), results->contextStack(), name);
            whenRunner.run(runTest, forward<Args>(args)...);

            auto stats = whenRunner.stats();

            results->endGiven(stats, name);

            return stats;
        }
    };

    // Workaround for MSVC bug. We should be able to use std forward, but it doesn't work for decayed arrays.
    template<typename T>
    struct Forward {
        static constexpr T&& run( typename std::remove_reference<T>::type& t ) {
            return std::forward<T>(t);
        }

        static constexpr T&& run( typename std::remove_reference<T>::type&& t ) {
            return std::forward<T>(t);
        }
    };

    template<typename T>
    struct Forward<T*> {
        static T* run(T* t ) {
            return t;
        }
    };

    template<typename Functor, typename... Args>
    unique_ptr<Context> given(string name, Functor runTest, Args&&... args) {
        return make_unique<Runner<RunTest<Functor, DecayArrayAndFunction_t<Args>...>, DecayArrayAndFunction_t<Args>...>>(
                move(name),
                RunTest<Functor, DecayArrayAndFunction_t<Args>...>(runTest),
                Forward<DecayArrayAndFunction_t<Args>>::run(args)...
            );
    }

    template<typename Functor, typename... Args>
    class RunExhaustive final: public NoCopy {
        Functor runTest;
        StoreArgs<Args...> args;

        template<typename BoundFunctor>
        static Stats exhaustive(BoundFunctor&& functor,
                                const string& name,
                                Out<ContextResultsRecorder> results,
                                Out<Results> test)
        {
            WhenRunner whenRunner(out(*test), results->contextStack(), name);
            whenRunner.run(forward<BoundFunctor>(functor));

            return whenRunner.stats();
        }

        template<typename BoundFunctor, typename Container, typename... BoundArgs>
        static Stats exhaustive(
                BoundFunctor&& functor,
                const string& name,
                Out<ContextResultsRecorder> results,
                Out<Results> test,
                const Container& container,
                const BoundArgs&... tail
            )
        {
            Stats stats;

            for (const auto& value : container) {
                stats += exhaustive(
                        [&] (Check& check, auto&&... args) {
                            functor(ref(check), value, args...);
                        },
                        name,
                        results,
                        test,
                        tail...
                    );
            }

            return stats;
        }
    public:
        RunExhaustive(Functor runTest, StoreArgs<Args...> args) : runTest(move(runTest)), args(move(args)) {}

        Stats operator()(const string& name, Out<ContextResultsRecorder> results) {
            auto test = results->beginGiven(name);

            Stats stats = args.apply([&] (const Args&... extractedArgs) {
                return exhaustive(move(runTest), name, results, out(*test), extractedArgs...);
            });

            results->endGiven(stats, name);

            return stats;
        }
    };

    template<typename Value, typename... Args>
    vector<Value> choice(Value value, Args&&... tail) {
        return vector<Value>{value, forward<Args>(tail)...};
    }

    template<typename Value>
    vector<Value> constant(Value value) {
        return vector<Value>(1, value);
    }

    template<typename... Args>
    class Exhaustive final: public NoCopy {
        StoreArgs<Args...> args;
    public:
        Exhaustive(Args&&... args) : args(forward<Args>(args)...) {}

        template<typename Functor>
        unique_ptr<Context> given(string name, Functor runTest) {
            return make_unique<Runner<RunExhaustive<Functor, Args...>>>(
                    move(name),
                    RunExhaustive<Functor, Args...>(move(runTest), move(args))
            );
        }
    };

    template<typename... Args>
    Exhaustive<Args...> exhaustive(Args&&... args) {
        return Exhaustive<DecayArrayAndFunction_t< Args>...>(forward<DecayArrayAndFunction_t< Args>>(args)...);
    }

    inline void list(const PathList& pathList, Out<Results> results) {
        Register::list(pathList, results);
    }

    inline bool run(const PathList& pathList, Out<Results> results) {
        auto stats = Register::run(pathList, results);

        return stats.failedTests() == 0 && stats.failedChecks() == 0;
    }

    inline void list(const PathList& pathList, Verbosity verbosity) {
        HumanResults results(out(cout), verbosity);
        return list(pathList, out(results));
    }

    inline bool run(const PathList& pathList, Verbosity verbosity) {
        HumanResults results(out(cout), verbosity);
        return run(pathList, out(results));
    }
}}}}

namespace Enhedron { namespace Test {
    using namespace Assertion;

    using Impl::Impl_Suite::Suite;
    using Impl::Impl_Suite::context;
    using Impl::Impl_Suite::Check;
    using Impl::Impl_Suite::given;
    using Impl::Impl_Suite::Exhaustive;
    using Impl::Impl_Suite::exhaustive;
    using Impl::Impl_Suite::choice;
    using Impl::Impl_Suite::constant;
    using Impl::Impl_Suite::list;
    using Impl::Impl_Suite::run;
}}
// File: Enhedron/Util/Math.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//


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
// File: Enhedron/CommandLine/Parameters.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//



#include <string>
#include <ostream>
#include <vector>
#include <set>
#include <map>
#include <algorithm>
#include <iterator>
#include <stdexcept>
#include <utility>
#include <iostream>
#include <sstream>
#include <iterator>

namespace Enhedron { namespace CommandLine { namespace Impl { namespace Impl_Parameters {
    using Util::bindFirst;
    using Util::optional;
    using Util::mapParameterPack;
    using Util::makeDivisibleByRoundingUp;

    using std::string;
    using std::ostream;
    using std::vector;
    using std::set;
    using std::map;
    using std::string;
    using std::move;
    using std::back_inserter;
    using std::logic_error;
    using std::index_sequence_for;
    using std::forward;
    using std::cout;
    using std::cerr;
    using std::runtime_error;
    using std::exception;
    using std::ostringstream;
    using std::min;
    using std::max;
    using std::fill_n;
    using std::ostream_iterator;
    using std::copy;

    enum class ExitStatus {
        OK,
        USAGE = 64,   // command line usage error 
        DATAERR = 65,    // data format error 
        NOINPUT = 66,    // cannot open input 
        NOUSER = 67,    // addressee unknown 
        NOHOST = 68,    // host name unknown 
        UNAVAILABLE = 69,    // service unavailable 
        SOFTWARE = 70,    // internal software error 
        OSERR = 71,    // system error (e.g., can't fork) 
        OSFILE = 72,    // critical OS file missing 
        CANTCREAT = 73,    // can't create (user) output file 
        IOERR = 74,    // input/output error 
        TEMPFAIL = 75,    // temp failure; user is invited to retry 
        PROTOCOL = 76,    // remote error in protocol 
        NOPERM = 77,    // permission denied 
        CONFIG = 78    // configuration error 
    };

    class Name: public NoCopy {
        optional<string> shortName_;
        string longName_;
        optional<string> description_;
    public:
        Name(string longName) : longName_("--" + longName) {}
        Name(string longName, string description) : longName_("--" + longName), description_(move(description)) {}

        Name(char shortName, string longName) :
                shortName_("-"), longName_("--" + longName)
        {
            shortName_->push_back(shortName);
        }

        Name(char shortName, string longName, string description) :
                shortName_("-"), longName_("--" + longName), description_(move(description))
        {
            shortName_->push_back(shortName);
        }

        template<typename Functor>
        void forEachName(Functor&& functor) const {
            if (shortName_) {
                functor(*shortName_);
            }

            functor(longName_);
        }

        bool anyMatch(const char* arg) const {
            if (shortName_) {
                if (arg == *shortName_) {
                    return true;
                }
            }

            return arg == longName_;
        }

        const string& longName() const { return longName_; }

        string makeNamesString() const {
            string result("  ");

            if (shortName_) {
                result += *shortName_ + ", ";
            }

            return result + longName_;
        }

        bool multiLineDescription(size_t width) const {
            return description_ && description_->size() > width;
        }

        void showDescription(Out<ostream> output, size_t width, size_t padding) const {
            width = max(width, static_cast<size_t>(10u));

            if (description_) {
                auto current = description_->begin();

                while (static_cast<size_t>(description_->end() - current) > width) {
                    auto currentEnd = current + static_cast<string::difference_type>(width);
                    auto breakAt = currentEnd;

                    while (true) {
                        --breakAt;

                        if (breakAt == current) {
                            // We didn't find a space - hyphenate.
                            --currentEnd;
                            copy(current, currentEnd, ostream_iterator<char>(*output));
                            *output << "-";
                            current = currentEnd;
                            break;
                        }

                        if (*breakAt == ' ') {
                            copy(current, breakAt, ostream_iterator<char>(*output));
                            current = breakAt;

                            break;
                        }
                    }

                    *output << "\n";
                    fill_n(ostream_iterator<char>(*output), padding, ' ');

                    while (*current == ' ') {
                        ++current;
                    }
                }

                copy(current, description_->end(), ostream_iterator<char>(*output));
            }

            *output << "\n";
        }
    };

    template<typename ValueType>
    class Option final {
        Name name_;
        string valueName_;
        optional<string> defaultValue_;
    public:
        using Value = ValueType;

        Option(Name name, string valueName) : name_(move(name)), valueName_(move(valueName)) {}

        Option(Name name, string valueName, string defaultValue) :
                name_(move(name)), valueName_(move(valueName)), defaultValue_(move(defaultValue)) {}

        template<typename Functor>
        void forEachName(Functor&& functor) const {
            name_.forEachName(forward<Functor>(functor));
        }

        bool anyMatch(const char* arg) const {
            return name_.anyMatch(arg);
        }

        const string& longName() const { return name_.longName(); }

        string makeNamesString() const {
            return name_.makeNamesString() + " <" + valueName_ + ">";
        }

        bool multiLineDescription(size_t width) const {
            return name_.multiLineDescription(width);
        }

        void showDescription(Out<ostream> output, size_t width, size_t padding) const {
            name_.showDescription(output, width, padding);
        }

        optional<string> defaultValue() const { return defaultValue_; }
    };

    class Flag final: public Name {
    public:
        using Name::Name;
    };

    enum class ParamType {
        OPTION,
        FLAG
    };

    class Arguments final : public NoCopy {
        Out<ostream> helpOut_;
        Out<ostream> errorOut_;
        string description_;
        string notes_;
        string version_;
        string positionalDescription_;
        size_t terminalWidth_;

        static const Flag& helpFlag() {
            static const Flag instance{"help", "Display this help message."};
            return instance;
        }

        static const Flag& versionFlag() {
            static const Flag instance{"version", "Display version information."};
            return instance;
        }

        template <typename... Params>
        void displayHelp(
                const char *exeName,
                Params&&... params
        ) {
            *helpOut_ << "Usage: " << exeName << " [OPTION]...";

            if ( ! positionalDescription_.empty()) {
                *helpOut_ << " [" << positionalDescription_ << "]..."; 
            }
            
            *helpOut_ << "\n\n";
            
            if ( ! description_.empty()) {
                *helpOut_ << description_ << "\n\n";
            }

            size_t padding = 0;

            mapParameterPack(
                    [this, &padding](const auto &arg) {
                        padding = max(arg.makeNamesString().size(), padding);
                    },
                    params...,
                    helpFlag(),
                    versionFlag()
            );

            constexpr const size_t tabWidth = 4;
            padding += tabWidth;
            padding = makeDivisibleByRoundingUp(padding, tabWidth);
            padding = min(terminalWidth_ / 2, padding);

            mapParameterPack(
                    [this, padding, tabWidth](const auto &arg) {
                        auto nameString = arg.makeNamesString();
                        *helpOut_ << nameString;
                        size_t currentPadding = padding;
                        bool descriptionOnNewline = nameString.size() + tabWidth > padding;

                        if (descriptionOnNewline) {
                            *helpOut_ << "\n";
                        }
                        else {
                            currentPadding -= nameString.size();
                        }

                        fill_n(ostream_iterator<char>(*helpOut_), currentPadding, ' ');
                        auto descriptionWidth = terminalWidth_ - padding;

                        arg.showDescription(helpOut_, descriptionWidth, padding);

                        if (descriptionOnNewline || arg.multiLineDescription(descriptionWidth)) {
                            *helpOut_ << "\n";
                        }
                    },
                    params...,
                    helpFlag(),
                    versionFlag()
            );

            *helpOut_ << "\n";

            if ( ! notes_.empty()) {
                *helpOut_ << notes_ << "\n\n";
            }
        }

        template<typename Functor>
        ExitStatus runImpl(
                map<string, vector<string>> optionValues,
                vector<string> positionalArgs,
                set<string> setFlags,
                Functor&& functor
            )
        {
            return functor(move(positionalArgs));
        }

        template<typename Functor, typename... ParamTail>
        ExitStatus runImpl(
                map<string, vector<string>> optionValues,
                vector<string> positionalArgs,
                set<string> setFlags,
                Functor&& functor,
                Option<string>&& param,
                ParamTail&&... paramTail
        )
        {
            vector<string> paramValues;

            param.forEachName([&] (const string& name) {
                const auto& newValues = optionValues[name];
                paramValues.insert(paramValues.end(), newValues.begin(), newValues.end());
            });

            string value;

            if (paramValues.empty()) {
                if (param.defaultValue()) {
                    value = *param.defaultValue();
                }
                else {
                    *errorOut_ << "Error: No value for " + param.longName() << "\n";

                    return ExitStatus::CONFIG;
                }
            }
            else {
                value = paramValues.front();
            }

            if (paramValues.size() > 1) {
                *errorOut_ << "Error: Multiple values for " + param.longName() << "\n";

                return ExitStatus::CONFIG;
            }

            return runImpl(
                    move(optionValues),
                    move(positionalArgs),
                    move(setFlags),
                    bindFirst(
                            forward<Functor>(functor),
                            move(value),
                            index_sequence_for<Option<string>, ParamTail...>()
                    ),
                    forward<ParamTail>(paramTail)...
            );
        }

        template<typename Functor, typename... ParamTail>
        ExitStatus runImpl(
                map<string, vector<string>> optionValues,
                vector<string> positionalArgs,
                set<string> setFlags,
                Functor&& functor,
                Option<vector<string>>&& param,
                ParamTail&&... paramTail
        )
        {
            vector<string> paramValues;

            param.forEachName([&] (const string& name) {
                const auto& newValues = optionValues[name];
                paramValues.insert(paramValues.end(), newValues.begin(), newValues.end());
            });

            return runImpl(
                    move(optionValues),
                    move(positionalArgs),
                    move(setFlags),
                    bindFirst(
                            forward<Functor>(functor),
                            move(paramValues),
                            index_sequence_for<Option<string>, ParamTail...>()
                    ),
                    forward<ParamTail>(paramTail)...
            );
        }

        template<typename Functor, typename... ParamTail>
        ExitStatus runImpl(
                map<string, vector<string>> optionValues,
                vector<string> positionalArgs,
                set<string> setFlags,
                Functor&& functor,
                Flag&& flag,
                ParamTail&&... paramTail
        )
        {
            bool flagValue = false;

            flag.forEachName([&] (const string& name) {
                flagValue |= setFlags.count(name) > 0;
            });

            return runImpl(
                    move(optionValues),
                    move(positionalArgs),
                    move(setFlags),
                    bindFirst(
                            forward<Functor>(functor),
                            flagValue,
                            index_sequence_for<Flag, ParamTail...>()
                    ),
                    forward<ParamTail>(paramTail)...
            );
        }

        template<typename OptionType, typename... ParamsTail>
        void readNamesImpl(
                Out<set<string>> optionNames,
                Out<set<string>> allNames,
                const Option<OptionType>& option,
                const ParamsTail&... paramsTail
        )
        {
            option.forEachName([&] (const string& name) {
                optionNames->emplace(name);
            });

            readNames(optionNames, allNames, paramsTail...);
        }

        template<typename... ParamsTail>
        void readNamesImpl(
                Out<set<string>> optionNames,
                Out<set<string>> allNames,
                const Flag& flag,
                const ParamsTail&... paramsTail
        )
        {
            readNames(optionNames, allNames, paramsTail...);
        }

        void readNames(Out<set<string>> optionNames, Out<set<string>> allNames) {}

        template<typename ParamType, typename... ParamsTail>
        void readNames(
                Out<set<string>> optionNames,
                Out<set<string>> allNames,
                const ParamType& param,
                const ParamsTail&... paramsTail
        )
        {
            param.forEachName([&] (const string& name) {
                if ( ! allNames->emplace(name).second) {
                    throw logic_error("Duplicate name " + name);
                }
            });

            readNamesImpl(optionNames, allNames, param, paramsTail...);
        }

        enum class StandardArg {
            NONE,
            HELP,
            VERSION
        };

        StandardArg checkArgs(int argc, const char* const argv[]) {
            if (argc <= 0) {
                throw runtime_error("argc is 0.");
            }
            else if (argv == nullptr) {
                throw runtime_error("argv is null.");
            }
            else {
                for (int index = 0; index < argc; ++index) {
                    if (argv[index] == nullptr) {
                        throw runtime_error("argv has null value.");
                    }
                    else {
                        if (helpFlag().anyMatch(argv[index])) {
                            return StandardArg::HELP;
                        }

                        if (versionFlag().anyMatch(argv[index])) {
                            return StandardArg::VERSION;
                        }
                    }
                }
            }

            return StandardArg::NONE;
        }

        template<typename Functor, typename... Params>
        ExitStatus runImpl(
                int argc, const char* const argv[],
                Functor &&functor,
                Params&&... params
        )
        {
            auto standardArg = checkArgs(argc, argv);

            if (standardArg == StandardArg::HELP) {
                displayHelp(argv[0], forward<Params>(params)...);
                return ExitStatus::OK;
            }

            if (standardArg == StandardArg::VERSION) {
                *helpOut_ << version_ << "\n";
                return ExitStatus::OK;
            }

            set<string> optionNames;
            set<string> allNames;
            readNames(out(optionNames), out(allNames), params...);

            map<string, vector<string>> optionValues;
            vector<string> positionalArgs;
            set<string> setFlags;

            for (int index = 1; index < argc; ++index) {
                string currentArg(argv[index]);

                if (currentArg == "--") {
                    positionalArgs.insert(positionalArgs.end(), argv + index, argv + argc);
                    break;
                }

                if ( ! currentArg.empty() && currentArg[0] == '-') {
                    if (allNames.count(currentArg) == 0) {
                        *errorOut_<< "Error: Unknown option " << currentArg << "\n";

                        return ExitStatus::USAGE;
                    }

                    if (optionNames.count(currentArg)) {
                        ++index;

                        if (index == argc) {
                            *errorOut_<< "Error: No value supplied for option " << currentArg << "\n";

                            return ExitStatus::USAGE;
                        }

                        optionValues[currentArg].emplace_back(argv[index]);
                    }
                    else {
                        setFlags.emplace(currentArg);
                    }
                }
                else {
                    positionalArgs.emplace_back(currentArg);
                }
            }

            return runImpl(
                move(optionValues),
                move(positionalArgs),
                move(setFlags),
                forward<Functor>(functor),
                forward<Params>(params)...
            );
        }

        const char* exeName(int argc, const char* const argv[]) {
            if (argv && argc > 0 && argv[0]) {
                return argv[0];
            }

            return "unknown";
        }
    public:
        Arguments(Out<ostream> helpOut, Out<ostream> errorOut, string version, size_t terminalWidth = 80) :
            helpOut_(helpOut), errorOut_(errorOut), version_(move(version)), terminalWidth_(terminalWidth)
        {}

        void setDescription(string description) { description_ = move(description); }
        void setNotes(string notes) { notes_ = move(notes); }
        void setPositionalDescription(string positionalDescription) {
            positionalDescription_ = move(positionalDescription);
        }

        Arguments(string version, size_t terminalWidth = 80) :
            Arguments(out(cout), out(cerr), move(version), terminalWidth)
        {}

        template<typename Functor, typename... Params>
        int run(
                int argc, const char* const argv[],
                Functor &&functor,
                Params&&... params
            )
        {
            try {
                return static_cast<int>(runImpl(argc, argv, forward<Functor>(functor), forward<Params>(params)...));
            }
            catch (const exception& e) {
                *errorOut_ << exeName(argc, argv) << ": " << e.what() << "\n";
            }

            return static_cast<int>(ExitStatus::SOFTWARE);
        }
    };
}}}}

namespace Enhedron { namespace CommandLine {
    using Impl::Impl_Parameters::ExitStatus;
    using Impl::Impl_Parameters::Arguments;
    using Impl::Impl_Parameters::Option;
    using Impl::Impl_Parameters::Flag;
    using Impl::Impl_Parameters::Name;
}}
// File: Enhedron/Test/Harness.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//



#include <string>
#include <vector>
#include <utility>
#include <algorithm>
#include <locale>
#include <cctype>
#include <regex>
#include <memory>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Harness {
    using std::string;
    using std::move;
    using std::vector;
    using std::shared_ptr;
    using std::make_shared;
    using std::runtime_error;
    using std::find_first_of;
    using std::tolower;
    using std::locale;
    using std::transform;
    using std::regex;

    using CommandLine::ExitStatus;
    using CommandLine::Flag;
    using CommandLine::Option;
    using CommandLine::Name;
    using CommandLine::Arguments;

    inline Verbosity parseVerbosity(string v) {
        transform(v.begin(), v.end(), v.begin(),
                  [](char c) { return tolower(c, locale()); } );

        if (v == "silent") return Verbosity::SILENT;
        if (v == "summary") return Verbosity::SUMMARY;
        if (v == "contexts") return Verbosity::CONTEXTS;
        if (v == "fixtures") return Verbosity::FIXTURES;
        if (v == "sections") return Verbosity::SECTIONS;
        if (v == "exhaustive_sections") return Verbosity::EXHAUSTIVE_SECTIONS;
        if (v == "checks") return Verbosity::CHECKS;
        if (v == "checks_expression") return Verbosity::CHECKS_EXPRESSION;
        if (v == "variables") return Verbosity::VARIABLES;

        throw runtime_error("Unknown verbosity \"" + v + "\"");
    }

    inline ExitStatus runTests(bool listOnly, string verbosityString, vector<string> pathList) {
        vector<shared_ptr<vector<regex>>> pathRegexs;

        for (auto& path : pathList) {
            vector<regex> pathComponentList;
            auto separatorPos = path.begin();
            auto matchChars = "/\\";
            auto matchCharsEnd = matchChars + 2;
            auto pathEnd = path.end();

            while (true) {
                auto end = find_first_of(separatorPos, pathEnd, matchChars, matchCharsEnd);

                while (end != pathEnd && end + 1 != pathEnd && *end == '\\') {
                    ++end;

                    if (*end == '/') {
                        copy(end, pathEnd, end - 1);
                    }
                    else {
                        ++end;
                    }

                    end = find_first_of(end, pathEnd, matchChars, matchCharsEnd);
                }

                if (separatorPos != end) {
                    pathComponentList.emplace_back(separatorPos, end);
                }

                if (end == pathEnd) {
                    break;
                }

                separatorPos = end;
                ++separatorPos;
            }

            pathRegexs.emplace_back(make_shared<vector<regex>>(move(pathComponentList)));
        }

        Verbosity verbosity = parseVerbosity(verbosityString);

        if (listOnly) {
            Test::list(pathRegexs, verbosity);
        }
        else {
            if ( ! Test::run(pathRegexs, verbosity)) {
                return ExitStatus::SOFTWARE;
            }
        }

        return ExitStatus::OK;
    }

    inline int run(int argc, const char* argv[]) {
        Arguments args("MosquitoNet test harness version 0.0.0");
        args.setDescription("Test Harness");
        args.setPositionalDescription("TEST_PATH");

        return args.run(
                argc, argv, runTests,
                Flag('l', "list", "List tests instead of running them."),
                Option<string>(Name('v', "verbosity", "Set the verbosity"),
                               "silent|summary|contexts|fixtures|sections|exhaustive-sections|checks_description|checks_expression|variables",
                               "contexts"
                )
        );
    }
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Harness::run;
}}
// File: Enhedron/Assertion/Functions.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//



#include <algorithm>
#include <utility>
#include <type_traits>

namespace Enhedron { namespace Assertion { namespace Impl { namespace Impl_Functions {
    using std::forward;
    using std::count;
    using std::count_if;
    using std::equal;
    using std::all_of;
    using std::any_of;
    using std::none_of;
    using std::search;
    using std::remove_reference_t;

    template<typename Container, typename Value>
    auto countEqual(Container&& container, Value&& value) {
        return makeFunction(
                "countEqual",
                [] (auto&& container, auto&& value) {
                    return count(container.begin(), container.end(), forward<decltype(value)>(value));
                }
        )(forward<Container>(container), forward<Value>(value));
    }

    template<typename Container, typename Predicate>
    auto countMatching(Container&& container, Predicate&& predicate) {
        return makeFunction(
                "countMatching",
                [] (auto&& container, auto&& predicate) {
                    return count_if(container.begin(), container.end(), forward<decltype(predicate)>(predicate));
                }
        )(forward<Container>(container), forward<Predicate>(predicate));
    }

    template<typename Container, typename Predicate>
    auto allOf(Container&& container, Predicate&& predicate) {
        return makeFunction(
                "allOf",
                [] (auto&& container, auto&& predicate) {
                    return all_of(container.begin(), container.end(), forward<decltype(predicate)>(predicate));
                }
        )(forward<Container>(container), forward<Predicate>(predicate));
    }

    template<typename Container, typename Predicate>
    auto anyOf(Container&& container, Predicate&& predicate) {
        return makeFunction(
                "anyOf",
                [] (auto&& container, auto&& predicate) {
                    return any_of(container.begin(), container.end(), forward<decltype(predicate)>(predicate));
                }
        )(forward<Container>(container), forward<Predicate>(predicate));
    }

    template<typename Container, typename Predicate>
    auto noneOf(Container&& container, Predicate&& predicate) {
        return makeFunction(
                "noneOf",
                [] (auto&& container, auto&& predicate) {
                    return none_of(container.begin(), container.end(), forward<decltype(predicate)>(predicate));
                }
        )(forward<Container>(container), forward<Predicate>(predicate));
    }

    template<typename Container>
    auto length(Container&& container) {
        return makeFunction(
                "length",
                [] (auto&& container) {
                    return container.size();
                }
        )(forward<Container>(container));
    }

    template<typename MainContainer, typename StartsWithContainer>
    auto startsWith(MainContainer&& container, StartsWithContainer&& starting) {
        return makeFunction(
                "startsWith",
                [] (auto&& container, auto&& starting) {
                    return container.size() >= starting.size() &&
                           equal(starting.begin(), starting.end(), container.begin());
                }
        )(forward<MainContainer>(container), forward<StartsWithContainer>(starting));
    }

    template<typename MainContainer, typename EndsWithContainer>
    auto endsWith(MainContainer&& container, EndsWithContainer&& ending) {
        return makeFunction(
                "endsWith",
                [] (auto&& container, auto&& ending) {
                    using ContainerType = remove_reference_t<decltype(container)>;
                    auto size = static_cast<typename ContainerType::difference_type>(ending.size());
                    return container.size() >= ending.size() &&
                            equal(ending.begin(), ending.end(), container.end() - size);
                }
        )(forward<MainContainer>(container), forward<EndsWithContainer>(ending));
    }

    template<typename MainContainer, typename Contained>
    auto contains(MainContainer&& mainContainer, Contained&& contained) {
        return makeFunction(
                "contains",
                [] (auto&& mainContainer, auto&& contained) {
                    return mainContainer.end() !=
                           search(mainContainer.begin(), mainContainer.end(), contained.begin(), contained.end());
                }
        )(forward<MainContainer>(mainContainer), forward<Contained>(contained));
    }
}}}}

namespace Enhedron { namespace Assertion {
    using Impl::Impl_Functions::countEqual;
    using Impl::Impl_Functions::countMatching;
    using Impl::Impl_Functions::allOf;
    using Impl::Impl_Functions::anyOf;
    using Impl::Impl_Functions::noneOf;
    using Impl::Impl_Functions::length;
    using Impl::Impl_Functions::startsWith;
    using Impl::Impl_Functions::endsWith;
    using Impl::Impl_Functions::contains;
}}
// File: Enhedron/Test.h
//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//


#endif /* ENHEDRON_MOSQUITONET_H_ */


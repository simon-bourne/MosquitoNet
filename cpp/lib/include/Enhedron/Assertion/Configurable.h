// Copyright 2015 Simon Bourne.

#pragma once

#include "Enhedron/Util/MetaProgramming.h"
#include "Enhedron/Util/Optional.h"

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

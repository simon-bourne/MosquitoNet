// Copyright 2015 Simon Bourne.

#pragma once

#include "Enhedron/Util/MetaProgramming.h"

#include <type_traits>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
#include <stdexcept>
#include <functional>
#include<tuple>

namespace Enhedron { namespace Assertion {
    template<typename Value, typename Enable = void>
    struct Convert {
        static std::string toString(const Value &value) {
            std::ostringstream valueString;
            valueString << value;

            return valueString.str();
        }
    };

    template<>
    struct Convert<std::nullptr_t> {
        static inline std::string toString(const std::nullptr_t &) {
            return "nullptr";
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
}}

namespace Enhedron { namespace Assertion { namespace Impl { namespace Configurable {
    using Util::StoreArgs;
    using Util::mapTuple;
    using Util::mapParameterPack;
    using Util::extractParameterPack;

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
    using std::decay_t;
    using std::tuple;
    using std::tie;
    using std::add_lvalue_reference_t;
    using std::index_sequence;
    using std::index_sequence_for;
    using std::get;
    using std::reference_wrapper;
    using std::ref;
    using std::cref;
    using std::is_function;

    class Expression {
    };

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
        using ResultType = decay_t<const Value>;

        explicit Literal(const Value &value) : value(value) { }

        string makeName() const {
            return Convert<Value>::toString(value);
        }

        void appendVariables(vector<Variable> &) const { }

        ResultType evaluate() const {
            return value;
        }

    private:
        const Value &value;
    };

    template<typename Functor, typename Arg>
    class UnaryOperator final : public Expression {
    public:
        using ResultType = decay_t<const result_of_t<Functor(typename Arg::ResultType)>>;

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

        ResultType evaluate() const {
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
        using ResultType = decay_t<const result_of_t<Functor(
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

        ResultType evaluate() const {
            return functor(lhs.evaluate(), rhs.evaluate());
        }

    private:
        const char *operatorName;
        Functor functor;
        Lhs lhs;
        Rhs rhs;
    };

    template<typename Functor, typename... Args>
    class Function final : public Expression {
    public:
        using ResultType = decay_t<const result_of_t<Functor&(Args...)>>;

        explicit Function(const char *name, Functor&& functor, const char *file, int line, Args&&... args) :
                name(name),
                functor(move(functor)),
                file(file),
                line(line),
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

            return valueString.str();
        }

        void appendVariables(vector<Variable>& variableList) const {
            variableList.emplace_back(name, "function", file, line);
        }

        ResultType evaluate() const {
            return extractParameterPack( [this] (const Args&... extractedArgs) {
                    return functor(extractedArgs...);
                },
                args
            );
        }
    private:
        template<typename Arg1, typename Arg2, typename... Tail>
        void makeParameterNames(Out<ostringstream> parameters, const Arg1& arg1, const Arg2& arg2, const Tail&... tail) const {
            (*parameters) << Convert<Arg1>::toString(arg1) << ", ";
            makeParameterNames(parameters, arg2, tail...);
        }

        template<typename Arg>
        void makeParameterNames(Out<ostringstream> parameters, const Arg& arg) const {
            (*parameters) << Convert<Arg>::toString(arg);
        }

        void makeParameterNames(Out<ostringstream> parameters) const {}

        template<typename Head, typename... Tail>
        void addParameterStrings() {}

        const char *name;
        Functor functor;
        const char* file;
        int line;
        tuple<Args...> args;
    };

    template<typename Value>
    class VariableRefExpression final : public Expression {
    public:
        using ResultType = decay_t<const Value>;

        explicit VariableRefExpression(const char *variableName, const Value& value, const char *file, int line) :
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

        ResultType evaluate() const {
            return value;
        }

        template<typename... Args>
        auto operator()(Args&&... args) {
            return Function<reference_wrapper<Value>, Args...>(
                    variableName, cref(value), file, line, forward<Args>(args)...
                );
        }

    private:
        const char *variableName;
        const Value& value;
        const char *file;
        int line;
    };

    template<typename Value>
    class VariableValueExpression final : public Expression {
    public:
        using ResultType = decay_t<const Value>;

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

        ResultType evaluate() const {
            return value;
        }

        // Only call this once! It will move the underlying value.
        template<typename... Args>
        Function<Value, Args...> operator()(Args&&... args) {
            return Function<Value, Args...>(variableName, move(value), file, line, forward<Args>(args)...);
        }

    private:
        const char *variableName;
        Value value;
        const char *file;
        int line;
    };

    template<typename Value>
    auto makeVariable(const char *name, const Value& value, const char *file, int line) {
        return VariableRefExpression<const Value>(name, value, file, line);
    }

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

    template<typename Arg>
    using IsExpression = enable_if_t<is_base_of<Expression, Arg>::value>*;

    template<typename Arg>
    using IsNotExpression = enable_if_t<!is_base_of<Expression, Arg>::value> *;

    template<typename Lhs, typename Rhs>
    using EitherIsExpression = enable_if_t<
            is_base_of<Expression, Lhs>::value || is_base_of<Expression, Rhs>::value> *;

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

    template<
            typename FailureHandler,
            typename Expression,
            typename SubExpression,
            typename... ContextVariableList>
    void ProcessFailureImpl(Expression expression, vector<Variable> variableList,
                        SubExpression variableExpression,
                        ContextVariableList... contextVariableList) {
        variableExpression.appendVariables(variableList);
        ProcessFailureImpl<FailureHandler>(move(expression), move(variableList), move(contextVariableList)...);
    }

    template<typename FailureHandler, typename Expression>
    void ProcessFailureImpl(Expression expression, vector<Variable> variableList) {
        FailureHandler::handleCheckFailure(expression.makeName(), move(variableList));
    }

    template<typename FailureHandler, typename Expression, typename... ContextVariableList>
    void ProcessFailure(Expression expression,
                        ContextVariableList... contextVariableList) {
        vector<Variable> variableList;
        expression.appendVariables(variableList);
        ProcessFailureImpl<FailureHandler>(move(expression), move(variableList), move(contextVariableList)...);
    }

    template<typename FailureHandler, typename Expression, typename... ContextVariableList>
    bool CheckWithFailureHandler(Expression expression, ContextVariableList... contextVariableList) {
        if (!static_cast<bool>(expression.evaluate())) {
            ProcessFailure<FailureHandler>(move(expression), move(contextVariableList)...);

            return false;
        }

        return true;
    }

    template<
            typename FailureHandler,
            typename Exception,
            typename Expression,
            typename... ContextVariableList,
            enable_if_t<is_same<Exception, exception>::value> * = nullptr
    >
    bool CheckThrowsWithFailureHandler(Expression expression, ContextVariableList... contextVariableList) {
        try {
            expression.evaluate();
            ProcessFailure<FailureHandler>(move(expression), move(contextVariableList)...);

            return false;
        }
        catch (const exception &e) {
            // Expected
        }

        return true;
    }

    template<
            typename FailureHandler,
            typename Exception,
            typename Expression,
            typename... ContextVariableList,
            enable_if_t<!is_same<Exception, exception>::value> * = nullptr
    >
    bool CheckThrowsWithFailureHandler(Expression expression, ContextVariableList... contextVariableList) {
        try {
            expression.evaluate();
            ProcessFailure<FailureHandler>(move(expression), move(contextVariableList)...);

            return false;
        }
        catch (const Exception &e) {
            // Expected
        }
        catch (const exception &e) {
            ProcessFailure<FailureHandler>(move(expression), move(contextVariableList)...);

            return false;
        }

        return true;
    }
}}}}

namespace Enhedron { namespace Assertion {
    using Impl::Configurable::CheckWithFailureHandler;
    using Impl::Configurable::CheckThrowsWithFailureHandler;
    using Impl::Configurable::IsExpression;
    using Impl::Configurable::Expression;
    using Impl::Configurable::Variable;
    using Impl::Configurable::ProcessFailure;
}}

#define M_ENHEDRON_VAR(expression) \
    (::Enhedron::Assertion::Impl::Configurable::makeVariable((#expression), (expression), (__FILE__), (__LINE__)))

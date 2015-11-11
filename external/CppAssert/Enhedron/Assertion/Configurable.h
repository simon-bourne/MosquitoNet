// Copyright 2015 Simon Bourne.

#pragma once

#include <type_traits>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
#include <stdexcept>
#include <functional>

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
    struct Convert<Value, typename std::enable_if<std::is_enum<Value>::value>::type> {
        static std::string toString(Value value) {
            std::ostringstream valueString;
            valueString << static_cast<typename std::underlying_type<Value>::type>(value);

            return valueString.str();
        }
    };
}}

namespace Enhedron { namespace Assertion { namespace Impl { namespace Configurable {
    using std::enable_if;
    using std::is_base_of;
    using std::is_same;
    using std::ostringstream;
    using std::string;
    using std::move;
    using std::vector;
    using std::result_of;
    using std::exception;
    using std::function;
    using std::pair;
    using std::decay_t;

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
        using ResultType = decay_t<const typename result_of<Functor(typename Arg::ResultType)>::type>;

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
        using ResultType = decay_t<const typename result_of<Functor(
                                typename Lhs::ResultType,
                                typename Rhs::ResultType)>::type>;

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

    template<typename Value>
    class VariableExpression final : public Expression {
    public:
        using ResultType = decay_t<const Value>;

        explicit VariableExpression(const char *variableName, const Value &value, const char *file, int line) :
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

    private:
        const char *variableName;
        const Value &value;
        const char *file;
        int line;
    };

    template<typename Functor>
    class ExceptionExpression final : public Expression {
    public:
        explicit ExceptionExpression(const char *expressionText, Functor expression, const char *file,
                                int line) :
                expressionText(expressionText),
                expression(move(expression)),
                file(file),
                line(line) { }

        string makeName() const {
            return expressionText;
        }

        void appendVariables(vector<Variable> &variableList) const {
            variableList.emplace_back(expressionText, what, file, line);
        }

        void evaluate() {
            try {
                expression();
            }
            catch (const exception& e) {
                what = e.what();
                throw;
            }
        }

    private:
        const char *expressionText;
        Functor expression;
        string what = "no exception";
        const char *file;
        int line;
    };

    template<typename Arg>
    using IsExpression = typename enable_if<is_base_of<Expression, Arg>::value>::type *;

    template<typename Arg>
    using IsNotExpression = typename enable_if<!is_base_of<Expression, Arg>::value>::type *;

    template<typename Lhs, typename Rhs>
    using EitherIsExpression = typename enable_if<
            is_base_of<Expression, Lhs>::value || is_base_of<Expression, Rhs>::value>::type *;

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

    template<typename Value>
    auto makeVariable(const char *name, const Value &value, const char *file, int line) {
        return VariableExpression<Value>(name, move(value), file, line);
    }

    template<typename Functor>
    auto makeException(const char *name, Functor functor, const char *file, int line) {
        return ExceptionExpression<Functor>(name, move(functor), file, line);
    }

    template<typename FailureHandler, typename Expression, typename VariableValue, typename... ContextVariableList>
    void ProcessFailureImpl(Expression expression, vector<Variable> variableList,
                        VariableExpression<VariableValue> variableExpression,
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
            typename enable_if<is_same<Exception, exception>::value>::type * = nullptr
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
            typename enable_if<!is_same<Exception, exception>::value>::type * = nullptr
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
    using Impl::Configurable::ExceptionExpression;
    using Impl::Configurable::IsExpression;
    using Impl::Configurable::Expression;
    using Impl::Configurable::Variable;
    using Impl::Configurable::VariableExpression;
    using Impl::Configurable::ProcessFailure;
}}

#define M_ENHEDRON_VAR(expression) \
    (::Enhedron::Assertion::Impl::Configurable::makeVariable((#expression), (expression), (__FILE__), (__LINE__)))

#define M_ENHEDRON_VOID(expression) \
    (::Enhedron::Assertion::Impl::Configurable::makeException((#expression), (expression), (__FILE__), (__LINE__)))

#ifdef NDEBUG
#define M_DEBUG(expression) \
        { static_cast<void>(0); }
#else
#define M_ENHEDRON_DEBUG(expression) \
        { expression; }
#endif // NDEBUG


#ifdef M_ENHEDRON_DO_DEBUG_EXTRA
#define M_DEBUG_EXTRA(expression) \
        { expression; }
#else
#define M_ENHEDRON_DEBUG_EXTRA(expression) \
        { static_cast<void>(0); }
#endif

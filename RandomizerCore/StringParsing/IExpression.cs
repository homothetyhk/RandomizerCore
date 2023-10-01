using System;
using System.Collections.Generic;
using System.Text;

namespace RandomizerCore.StringParsing
{
    // todo - all validation implementations should actually produce useful error messages
    public interface IExpression<T>
    {
        /// <summary>
        /// Speculates possible types for an expression
        /// </summary>
        IEnumerable<T> Evaluate();

        bool Validate();

        string Print();
    }

    public abstract record PrefixExpression<T>(OperatorToken Operator, IExpression<T> Operand) : IExpression<T>
    {
        public abstract IEnumerable<T> Evaluate();
        public abstract bool Validate();

        public string Print() => Operator.Print() + Operand.Print();
    }

    public abstract record PostfixExpression<T>(IExpression<T> Operand, OperatorToken Operator) : IExpression<T>
    {
        public abstract IEnumerable<T> Evaluate();
        public abstract bool Validate();

        public string Print() => Operand.Print() + Operator.Print();
    }

    public abstract record InfixExpression<T>(IExpression<T> Left, OperatorToken Operator, IExpression<T> Right) : IExpression<T>
    {
        public abstract IEnumerable<T> Evaluate();
        public abstract bool Validate();

        public string Print() => Left.Print() + Operator.Print() + Right.Print();
    }
}

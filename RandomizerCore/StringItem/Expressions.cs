using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Remoting.Messaging;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.StringItem
{
    public enum EvaluatedType
    {
        Int,
        Bool,
        TermLike,
        ItemEffect
    }

    // todo - all expressions should actually preserve their operator token in order to be able to reconstruct the source string
    // todo - all validation implementations should actually produce useful error messages
    public interface IExpression
    {
        /// <summary>
        /// Speculates possible types for an expression
        /// </summary>
        /// <returns></returns>
        IEnumerable<EvaluatedType> Evaluate();

        bool Validate();
    }

    public record AtomExpression(Token Token) : IExpression
    {
        public IEnumerable<EvaluatedType> Evaluate() => Token switch
        {
            NameToken => new[] { EvaluatedType.TermLike, EvaluatedType.Bool },
            NumberToken => new[] { EvaluatedType.Int },
            LogicStringToken => new[] { EvaluatedType.Bool },
            _ => throw new NotImplementedException(), // todo better exception
        };

        public bool Validate() => Token is NameToken or NumberToken or LogicStringToken;

        public static bool IsAtomToken(Token token) => new AtomExpression(token).Validate();
    }

    // ----- prefix expressions ----- //

    public record NegationExpression(IExpression Operand) : IExpression
    {
        public IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.Bool };

        public bool Validate() => Operand.Validate() && Operand.Evaluate().Contains(EvaluatedType.Bool);
    }

    public record ReferenceExpression(IExpression Operand) : IExpression
    {
        public IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect, EvaluatedType.Bool };

        public bool Validate() => Operand.Validate() && Operand.Evaluate().Contains(EvaluatedType.TermLike);
    }

    public static class PrefixExpression
    {
        public static IExpression ForOperand(OperatorToken op, IExpression operand) => op.Operator switch
        {
            Operators.Negation => new NegationExpression(operand),
            Operators.Reference => new ReferenceExpression(operand),
            _ => throw new NotImplementedException()
        };
    }

    // ----- postfix expressions ----- //

    public record CoalescingExpression(IExpression Operand) : IExpression
    {
        public IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public bool Validate() => Operand.Validate() && Operand.Evaluate().Contains(EvaluatedType.TermLike);
    }

    public record IncrementExpression(IExpression Operand) : IExpression
    {
        public IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public bool Validate() => Operand.Validate() && Operand.Evaluate().Contains(EvaluatedType.TermLike);
    }

    public static class PostfixExpression
    {
        public static IExpression ForOperand(OperatorToken op, IExpression operand) => op.Operator switch
        {
            Operators.TermCoalescing => new CoalescingExpression(operand),
            Operators.Increment => new IncrementExpression(operand),
            _ => throw new NotImplementedException()
        };
    }

    // ----- infix expressions ----- //

    public record AdditionAssignmentExpression(IExpression Left, IExpression Right) : IExpression
    {
        public IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public bool Validate() => Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(EvaluatedType.TermLike)
            && Right.Evaluate().Contains(EvaluatedType.Int);
    }

    public record MaxAssignmentExpression(IExpression Left, IExpression Right) : IExpression
    {
        public IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public bool Validate() => Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(EvaluatedType.TermLike)
            && Right.Evaluate().Contains(EvaluatedType.Int);
    }

    public record ConditionalExpression(IExpression Left, IExpression Right) : IExpression
    {
        public IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public bool Validate() => Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(EvaluatedType.Bool)
            && Right.Evaluate().Contains(EvaluatedType.ItemEffect);
    }

    public record ShortCircuitChainingExpression(IExpression Left, IExpression Right) : IExpression
    {
        public IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public bool Validate() => Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(EvaluatedType.ItemEffect)
            && Right.Evaluate().Contains(EvaluatedType.ItemEffect);
    }

    public record ChainingExpression(IExpression Left, IExpression Right) : IExpression
    {
        public IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public bool Validate() => Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(EvaluatedType.ItemEffect)
            && Right.Evaluate().Contains(EvaluatedType.ItemEffect);
    }

    public static class InfixExpression
    {
        public static IExpression ForOperands(OperatorToken op, IExpression left, IExpression right) => op.Operator switch
        {
            Operators.AdditionAssignment => new AdditionAssignmentExpression(left, right),
            Operators.MaxAssignment => new MaxAssignmentExpression(left, right),
            Operators.Conditional => new ConditionalExpression(left, right),
            Operators.ShortCircuitChaining => new ShortCircuitChainingExpression(left, right),
            Operators.Chaining => new ChainingExpression(left, right),
            _ => throw new NotImplementedException()
        };
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
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

    // todo - all validation implementations should actually produce useful error messages
    public interface IExpression
    {
        /// <summary>
        /// Speculates possible types for an expression
        /// </summary>
        /// <returns></returns>
        IEnumerable<EvaluatedType> Evaluate();

        bool Validate();

        string Print();
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

        public string Print() => Token.Print();

        public static bool IsAtomToken(Token token) => new AtomExpression(token).Validate();
    }

    public record GroupingExpression(StructuralToken OpenParenthesis, IExpression Nested, StructuralToken CloseParenthesis) : IExpression
    {
        public IEnumerable<EvaluatedType> Evaluate() => Nested.Evaluate();
        public bool Validate() => OpenParenthesis.TokenType == StructuralToken.Type.OpenParenthesis
            && CloseParenthesis.TokenType == StructuralToken.Type.CloseParenthesis
            && Nested.Validate();
        public string Print() => OpenParenthesis.Print() + Nested.Print() + CloseParenthesis.Print();
    }

    // ----- prefix expressions ----- //

    public abstract record PrefixExpression(OperatorToken Operator, IExpression Operand) : IExpression
    {
        public abstract IEnumerable<EvaluatedType> Evaluate();
        public abstract bool Validate();

        public string Print() => Operator.Print() + Operand.Print();

        public static IExpression ForOperand(OperatorToken op, IExpression operand) => op.Operator switch
        {
            Operators.Negation => new NegationExpression(op, operand),
            Operators.Reference => new ReferenceExpression(op, operand),
            _ => throw new NotImplementedException()
        };
    }

    public record NegationExpression(OperatorToken Operator, IExpression Operand) : PrefixExpression(Operator, Operand)
    {
        public override IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.Bool };

        public override bool Validate() => Operator.Operator == Operators.Negation 
            && Operand.Validate() && Operand.Evaluate().Contains(EvaluatedType.Bool);
    }

    public record ReferenceExpression(OperatorToken Operator, IExpression Operand) : PrefixExpression(Operator, Operand)
    {
        public override IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect, EvaluatedType.Bool };

        public override bool Validate() => Operator.Operator == Operators.Reference
            && Operand.Validate() && Operand.Evaluate().Contains(EvaluatedType.TermLike);
    }

    // ----- postfix expressions ----- //

    public abstract record PostfixExpression(IExpression Operand, OperatorToken Operator) : IExpression
    {
        public abstract IEnumerable<EvaluatedType> Evaluate();
        public abstract bool Validate();

        public string Print() => Operand.Print() + Operator.Print();

        public static IExpression ForOperand(OperatorToken op, IExpression operand) => op.Operator switch
        {
            Operators.TermCoalescing => new CoalescingExpression(operand, op),
            Operators.Increment => new IncrementExpression(operand, op),
            _ => throw new NotImplementedException()
        };
    }

    public record CoalescingExpression(IExpression Operand, OperatorToken Operator) : PostfixExpression(Operand, Operator)
    {
        public override IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.TermLike };

        public override bool Validate() => Operator.Operator == Operators.TermCoalescing
            && Operand.Validate() && Operand.Evaluate().Contains(EvaluatedType.TermLike);
    }

    public record IncrementExpression(IExpression Operand, OperatorToken Operator) : PostfixExpression(Operand, Operator)
    {
        public override IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public override bool Validate() => Operator.Operator == Operators.Increment
            && Operand.Validate() && Operand.Evaluate().Contains(EvaluatedType.TermLike);
    }

    // ----- infix expressions ----- //

    public abstract record InfixExpression(IExpression Left, OperatorToken Operator, IExpression Right) : IExpression
    {
        public abstract IEnumerable<EvaluatedType> Evaluate();
        public abstract bool Validate();

        public string Print() => Left.Print() + Operator.Print() + Right.Print();

        public static IExpression ForOperands(OperatorToken op, IExpression left, IExpression right) => op.Operator switch
        {
            Operators.AdditionAssignment => new AdditionAssignmentExpression(left, op, right),
            Operators.MaxAssignment => new MaxAssignmentExpression(left, op, right),
            Operators.Conditional => new ConditionalExpression(left, op, right),
            Operators.ShortCircuitChaining => new ShortCircuitChainingExpression(left, op, right),
            Operators.Chaining => new ChainingExpression(left, op, right),
            _ => throw new NotImplementedException()
        };
    }

    public record AdditionAssignmentExpression(IExpression Left, OperatorToken Operator, IExpression Right) : InfixExpression(Left, Operator, Right)
    {
        public override IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public override bool Validate() => Operator.Operator == Operators.AdditionAssignment
            && Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(EvaluatedType.TermLike)
            && Right.Evaluate().Contains(EvaluatedType.Int);
    }

    public record MaxAssignmentExpression(IExpression Left, OperatorToken Operator, IExpression Right) : InfixExpression(Left, Operator, Right)
    {
        public override IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public override bool Validate() => Operator.Operator == Operators.MaxAssignment
            && Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(EvaluatedType.TermLike)
            && Right.Evaluate().Contains(EvaluatedType.Int);
    }

    public record ConditionalExpression(IExpression Left, OperatorToken Operator, IExpression Right) : InfixExpression(Left, Operator, Right)
    {
        public override IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public override bool Validate() => Operator.Operator == Operators.Conditional
            && Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(EvaluatedType.Bool)
            && Right.Evaluate().Contains(EvaluatedType.ItemEffect);
    }

    public record ShortCircuitChainingExpression(IExpression Left, OperatorToken Operator, IExpression Right) : InfixExpression(Left, Operator, Right)
    {
        public override IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public override bool Validate() => Operator.Operator == Operators.ShortCircuitChaining
            && Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(EvaluatedType.ItemEffect)
            && Right.Evaluate().Contains(EvaluatedType.ItemEffect);
    }

    public record ChainingExpression(IExpression Left, OperatorToken Operator, IExpression Right) : InfixExpression(Left, Operator, Right)
    {
        public override IEnumerable<EvaluatedType> Evaluate() => new[] { EvaluatedType.ItemEffect };

        public override bool Validate() => Operator.Operator == Operators.Chaining
            && Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(EvaluatedType.ItemEffect)
            && Right.Evaluate().Contains(EvaluatedType.ItemEffect);
    }
}

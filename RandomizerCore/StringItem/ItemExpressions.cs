using RandomizerCore.StringParsing;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.StringItem
{
    public enum ItemExpressionType
    {
        Int,
        Bool,
        TermLike,
        ItemEffect
    }

    public record ItemAtomExpression(Token Token) : IExpression<ItemExpressionType>
    {
        public IEnumerable<ItemExpressionType> Evaluate() => Token switch
        {
            NameToken => new[] { ItemExpressionType.TermLike, ItemExpressionType.Bool },
            NumberToken => new[] { ItemExpressionType.Int },
            StringToken => new[] { ItemExpressionType.Bool },
            _ => throw new NotImplementedException()
        };

        public bool Validate() => Token is NameToken or NumberToken or StringToken;

        public string Print() => Token.Print();

        public static bool IsAtomToken(Token token) => new ItemAtomExpression(token).Validate();
    }

    // ----- prefix expressions ----- //

    public record NegationExpression(OperatorToken Operator, IExpression<ItemExpressionType> Operand) 
        : PrefixExpression<ItemExpressionType>(Operator, Operand)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.Bool };

        public override bool Validate() => Operator.Operator == ItemOperatorProvider.Negation 
            && Operand.Validate() && Operand.Evaluate().Contains(ItemExpressionType.Bool);
    }

    public record ReferenceExpression(OperatorToken Operator, IExpression<ItemExpressionType> Operand) 
        : PrefixExpression<ItemExpressionType>(Operator, Operand)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect, ItemExpressionType.Bool };

        public override bool Validate() => Operator.Operator == ItemOperatorProvider.Reference
            && Operand.Validate() && Operand.Evaluate().Contains(ItemExpressionType.TermLike);
    }

    // ----- postfix expressions ----- //

    public record CoalescingExpression(IExpression<ItemExpressionType> Operand, OperatorToken Operator) 
        : PostfixExpression<ItemExpressionType>(Operand, Operator)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.TermLike };

        public override bool Validate() => Operator.Operator == ItemOperatorProvider.TermCoalescing
            && Operand.Validate() && Operand.Evaluate().Contains(ItemExpressionType.TermLike);
    }

    public record IncrementExpression(IExpression<ItemExpressionType> Operand, OperatorToken Operator)
        : PostfixExpression<ItemExpressionType>(Operand, Operator)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate() => Operator.Operator == ItemOperatorProvider.Increment
            && Operand.Validate() && Operand.Evaluate().Contains(ItemExpressionType.TermLike);
    }

    // ----- infix expressions ----- //

    public record AdditionAssignmentExpression(IExpression<ItemExpressionType> Left, OperatorToken Operator, IExpression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate() => Operator.Operator == ItemOperatorProvider.AdditionAssignment
            && Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(ItemExpressionType.TermLike)
            && Right.Evaluate().Contains(ItemExpressionType.Int);
    }

    public record MaxAssignmentExpression(IExpression<ItemExpressionType> Left, OperatorToken Operator, IExpression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate() => Operator.Operator == ItemOperatorProvider.MaxAssignment
            && Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(ItemExpressionType.TermLike)
            && Right.Evaluate().Contains(ItemExpressionType.Int);
    }

    public record ConditionalExpression(IExpression<ItemExpressionType> Left, OperatorToken Operator, IExpression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate() => Operator.Operator == ItemOperatorProvider.Conditional
            && Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(ItemExpressionType.Bool)
            && Right.Evaluate().Contains(ItemExpressionType.ItemEffect);
    }

    public record ShortCircuitChainingExpression(IExpression<ItemExpressionType> Left, OperatorToken Operator, IExpression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate() => Operator.Operator == ItemOperatorProvider.ShortCircuitChaining
            && Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(ItemExpressionType.ItemEffect)
            && Right.Evaluate().Contains(ItemExpressionType.ItemEffect);
    }

    public record ChainingExpression(IExpression<ItemExpressionType> Left, OperatorToken Operator, IExpression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate() => Operator.Operator == ItemOperatorProvider.Chaining
            && Left.Validate() && Right.Validate()
            && Left.Evaluate().Contains(ItemExpressionType.ItemEffect)
            && Right.Evaluate().Contains(ItemExpressionType.ItemEffect);
    }
}

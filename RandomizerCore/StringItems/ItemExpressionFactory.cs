﻿using RandomizerCore.StringParsing;

namespace RandomizerCore.StringItems
{
    public class ItemExpressionFactory : IExpressionFactory<ItemExpressionType>
    {
        /// <summary>
        /// Atom expression which produces a no-op item effect.
        /// </summary>
        public const string EmptyEffect = "_";

        public bool IsAtom(Token token)
        {
            return ItemAtomExpression.IsAtomToken(token);
        }

        public IExpression<ItemExpressionType> CreateAtomExpression(Token token)
        {
            if (token is NameToken { Value: "_" }) return new EmptyEffectExpression(token);
            return new ItemAtomExpression(token);
        }

        public IExpression<ItemExpressionType> CreatePrefixExpression(OperatorToken op, IExpression<ItemExpressionType> operand)
        {
            return op.Operator switch
            {
                ItemOperatorProvider.Negation => new NegationExpression(op, operand),
                ItemOperatorProvider.Reference => new ReferenceExpression(op, operand),
                _ => throw new NotImplementedException()
            };
        }

        public IExpression<ItemExpressionType> CreatePostfixExpression(IExpression<ItemExpressionType> operand, OperatorToken op)
        {
            return op.Operator switch
            {
                ItemOperatorProvider.TermCoalescing => new CoalescingExpression(operand, op),
                ItemOperatorProvider.Increment => new IncrementExpression(operand, op),
                _ => throw new NotImplementedException()
            };
        }

        public IExpression<ItemExpressionType> CreateInfixExpression(IExpression<ItemExpressionType> left, OperatorToken op, IExpression<ItemExpressionType> right)
        {
            return op.Operator switch
            {
                ItemOperatorProvider.AdditionAssignment => new AdditionAssignmentExpression(left, op, right),
                ItemOperatorProvider.MaxAssignment => new MaxAssignmentExpression(left, op, right),
                ItemOperatorProvider.Conditional => new ConditionalExpression(left, op, right),
                ItemOperatorProvider.ShortCircuitChaining => new ShortCircuitChainingExpression(left, op, right),
                ItemOperatorProvider.Chaining => new ChainingExpression(left, op, right),
                _ => throw new NotImplementedException()
            };
        }
    }
}

using RandomizerCore.StringParsing;

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
            return token is NameToken or NumberToken or StringToken;
        }

        public Expression<ItemExpressionType> CreateAtomExpression(Token token)
        {
            if (token is NameToken { Content: "_" }) return new EmptyEffectExpression(token);
            return new ItemAtomExpression(token);
        }

        public Expression<ItemExpressionType> CreatePrefixExpression(OperatorToken op, Expression<ItemExpressionType> operand)
        {
            return op.Definition.Operator switch
            {
                ItemOperatorProvider.Negation => new NegationExpression(op, operand),
                ItemOperatorProvider.Reference => new ReferenceExpression(op, operand),
                _ => throw new NotImplementedException(op.Definition.Operator)
            };
        }

        public Expression<ItemExpressionType> CreatePostfixExpression(Expression<ItemExpressionType> operand, OperatorToken op)
        {
            return op.Definition.Operator switch
            {
                ItemOperatorProvider.TermCoalescing => new CoalescingExpression(operand, op),
                ItemOperatorProvider.Increment => new IncrementExpression(operand, op),
                _ => throw new NotImplementedException(op.Definition.Operator)
            };
        }

        public Expression<ItemExpressionType> CreateInfixExpression(Expression<ItemExpressionType> left, OperatorToken op, Expression<ItemExpressionType> right)
        {
            return op.Definition.Operator switch
            {
                ItemOperatorProvider.AdditionAssignment => new AdditionAssignmentExpression(left, op, right),
                ItemOperatorProvider.MaxAssignment => new MaxAssignmentExpression(left, op, right),
                ItemOperatorProvider.Conditional => new ConditionalExpression(left, op, right),
                ItemOperatorProvider.ShortCircuitChaining => new ShortCircuitChainingExpression(left, op, right),
                ItemOperatorProvider.Chaining => new ChainingExpression(left, op, right),
                _ => throw new NotImplementedException(op.Definition.Operator)
            };
        }
    }
}

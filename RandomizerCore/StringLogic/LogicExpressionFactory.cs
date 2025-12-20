using RandomizerCore.StringParsing;
using Op = RandomizerCore.StringParsing.OperatorToken;

namespace RandomizerCore.StringLogic
{
    public class LogicExpressionFactory : IExpressionFactory<LogicExpressionType>
    {
        public bool IsAtom(Token token)
        {
            return token is NumberToken || token is NameToken;
        }

        public Expression<LogicExpressionType> CreateAtomExpression(Token token)
        {
            if (token is NameToken n)
            {
                if (BoolLiteralExpression.IsConstAtom(n.Content))
                {
                    return new BoolLiteralExpression(n);
                }
                else if (NullLiteralExpression.IsNullAtom(n.Content))
                {
                    return new NullLiteralExpression(n);
                }
                else
                {
                    return new LogicAtomExpression(n);
                }
            }
            if (token is NumberToken num)
            {
                return new NumberLiteralExpression(num);
            }

            throw new NotImplementedException(token.ToString());
        }

        public Expression<LogicExpressionType> CreateInfixExpression(Expression<LogicExpressionType> left, Op op, Expression<LogicExpressionType> right)
        {
            return op.Definition.Operator switch
            {
                LogicOperatorProvider.AND => new AndExpression(left, op, right),
                LogicOperatorProvider.OR => new OrExpression(left, op, right),
                LogicOperatorProvider.COALESCE => new CoalesceExpression(left, op, right),
                LogicOperatorProvider.LT or LogicOperatorProvider.GT or LogicOperatorProvider.EQ => new ComparisonExpression(left, op, right),
                _ => throw new NotImplementedException()
            };
        }

        public Expression<LogicExpressionType> CreatePostfixExpression(Expression<LogicExpressionType> operand, Op op)
        {
            return op.Definition.Operator switch
            {
                LogicOperatorProvider.PROJECT => new ProjectionExpression(operand, op),
                _ => throw new NotImplementedException(op.Definition.Operator)
            };
        }

        public Expression<LogicExpressionType> CreatePrefixExpression(Op op, Expression<LogicExpressionType> operand)
        {
            return op.Definition.Operator switch
            {
                LogicOperatorProvider.REF => new ReferenceExpression(op, operand),
                _ => throw new NotImplementedException()
            };
        }
    }
}

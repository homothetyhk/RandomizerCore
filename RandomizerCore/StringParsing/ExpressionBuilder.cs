namespace RandomizerCore.StringParsing
{
    file static class StringParsingExtensions
    {
        internal static GroupingExpression<T> Group<T>(this Expression<T> expr) => new(
            new StructuralToken(StructuralToken.Types.OpenParenthesis),
            expr,
            new StructuralToken(StructuralToken.Types.CloseParenthesis)
            );
    }

    public class ExpressionBuilder<T>
    {
        private readonly IOperatorProvider operatorProvider;
        private readonly IExpressionFactory<T> expressionFactory;

        public ExpressionBuilder(IOperatorProvider operatorProvider, IExpressionFactory<T> expressionFactory)
        {
            this.operatorProvider = operatorProvider;
            this.expressionFactory = expressionFactory;
        }

        public Expression<T> ApplyInfixOperator(Expression<T> argL, OperatorToken op, Expression<T> argR)
        {
            int? pArgLeft = GetBindingPower(argL, false);
            int? pArgRight = GetBindingPower(argR, true);
            if (!op.Definition.IsInfix) throw new ArgumentException($"Unrecognized infix operator {op.Definition.Operator}.", nameof(op));

            if (pArgLeft.HasValue && pArgLeft.Value <= op.Definition.PostfixBindingPower.Value)
            {
                argL = argL.Group();
            }
            if (pArgRight.HasValue && pArgRight.Value <= op.Definition.PrefixBindingPower.Value)
            {
                argR = argR.Group();
            }

            return expressionFactory.CreateInfixExpression(argL, op, argR);
        }

        public Expression<T> ApplyInfixOperatorLeftAssoc(IEnumerable<Expression<T>> args, OperatorToken op)
        {
            return args.Aggregate((expr, operand) => ApplyInfixOperator(expr, op, operand));
        }

        public Expression<T> ApplyPrefixOperator(OperatorToken op, Expression<T> operand)
        {
            int? pOperand = GetBindingPower(operand, true);
            if (!op.Definition.IsPrefix) throw new ArgumentException($"Unrecognized prefix operator {op.Definition.Operator}.", nameof(op));

            if (pOperand.HasValue && pOperand.Value <= op.Definition.PrefixBindingPower.Value) operand = operand.Group();
            return expressionFactory.CreatePrefixExpression(op, operand);
        }

        public Expression<T> ApplyPostfixOperator(Expression<T> operand, OperatorToken op)
        {
            int? pOperand = GetBindingPower(operand, false);
            if (!op.Definition.IsPostfix) throw new ArgumentException($"Unrecognized postfix operator {op.Definition.Operator}.", nameof(op));

            if (pOperand.HasValue && pOperand.Value <= op.Definition.PostfixBindingPower.Value) operand = operand.Group();
            return expressionFactory.CreatePostfixExpression(operand, op);
        }

        private int? GetBindingPower(Expression<T> expr, bool relativeToLeft)
        {
            if (relativeToLeft)
            {
                return expr switch
                {
                    InfixExpression<T> i => i.Operator.Definition.PostfixBindingPower,
                    PostfixExpression<T> p => p.Operator.Definition.PostfixBindingPower,
                    _ => null
                };
            }
            else
            {
                return expr switch
                {
                    InfixExpression<T> i => i.Operator.Definition.PrefixBindingPower,
                    PrefixExpression<T> p => p.Operator.Definition.PrefixBindingPower,
                    _ => null
                };
            }
        }

        public Expression<T> NameAtom(string name) => expressionFactory.CreateAtomExpression(CreateTermToken(name));

        public Expression<T> NumberAtom(int i) => expressionFactory.CreateAtomExpression(CreateNumberToken(i));

        public Expression<T> StringAtom(string str) => expressionFactory.CreateAtomExpression(CreateStringToken(str));

        public Expression<T> NameOrNumberAtom(string name) => int.TryParse(name, out int i) ? NumberAtom(i) : NameAtom(name);

        public OperatorToken Op(string op)
        {
            return new(operatorProvider.GetDefinition(op) ?? throw new KeyNotFoundException($"Unrecognized operator {op}"));
        }

        private static NameToken CreateTermToken(string name) => new NameToken(name);

        private static NumberToken CreateNumberToken(int i) => new NumberToken(i);

        private static StringToken CreateStringToken(string str) => new('`', str);
    }
}

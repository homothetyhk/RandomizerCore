namespace RandomizerCore.StringParsing
{
    file static class StringParsingExtensions
    {
        internal static U WithEmptyTrivia<U>(this U u) where U : Token
        {
            u.LeadingTrivia = u.TrailingTrivia = string.Empty;
            return u;
        }

        internal static GroupingExpression<T> Group<T>(this IExpression<T> expr) => new(
            new StructuralToken { TokenType = StructuralToken.Type.OpenParenthesis }.WithEmptyTrivia(),
            expr,
            new StructuralToken { TokenType = StructuralToken.Type.CloseParenthesis }.WithEmptyTrivia()
            );
    }

    internal class ExpressionBuilder<T>
    {
        private readonly IOperatorProvider operatorProvider;
        private readonly IExpressionFactory<T> expressionFactory;

        public ExpressionBuilder(IOperatorProvider operatorProvider, IExpressionFactory<T> expressionFactory)
        {
            this.operatorProvider = operatorProvider;
            this.expressionFactory = expressionFactory;
        }

        public IExpression<T> ApplyInfixOperator(IExpression<T> argL, OperatorToken op, IExpression<T> argR)
        {
            int? pArgLeft = GetBindingPower(argL, false);
            int? pArgRight = GetBindingPower(argR, true);
            if (operatorProvider.InfixBindingPower(op.Operator) is not (int leftP, int rightP)) throw new ArgumentException($"Unrecognized infix operator {op.Operator}.", nameof(op));

            if (pArgLeft.HasValue && pArgLeft.Value <= leftP)
            {
                argL = argL.Group();
            }
            if (pArgRight.HasValue && pArgRight.Value <= rightP)
            {
                argR = argR.Group();
            }

            return expressionFactory.CreateInfixExpression(argL, op, argR);
        }

        public IExpression<T> ApplyInfixOperatorLeftAssoc(IEnumerable<IExpression<T>> args, OperatorToken op)
        {
            return args.Aggregate((expr, operand) => ApplyInfixOperator(expr, op, operand));
        }

        public IExpression<T> ApplyPrefixOperator(OperatorToken op, IExpression<T> operand)
        {
            int? pOperand = GetBindingPower(operand, true);
            if (operatorProvider.PrefixBindingPower(op.Operator) is not int power) throw new ArgumentException($"Unrecognized prefix operator {op.Operator}.", nameof(op));

            if (pOperand.HasValue && pOperand <= power) operand = operand.Group();
            return expressionFactory.CreatePrefixExpression(op, operand);
        }

        public IExpression<T> ApplyPostfixOperator(IExpression<T> operand, OperatorToken op)
        {
            int? pOperand = GetBindingPower(operand, false);
            if (operatorProvider.PostfixBindingPower(op.Operator) is not int power) throw new ArgumentException($"Unrecognized postfix operator {op.Operator}.", nameof(op));

            if (pOperand.HasValue && pOperand <= power) operand = operand.Group();
            return expressionFactory.CreatePostfixExpression(operand, op);
        }

        private int? GetBindingPower(IExpression<T> expr, bool relativeToLeft)
        {
            return expr switch
            {
                InfixExpression<T> i => operatorProvider.InfixBindingPower(i.Operator.Operator) is (int, int) pair ? relativeToLeft ? pair.Item1 : pair.Item2 : null,
                PostfixExpression<T> po => relativeToLeft ? operatorProvider.PostfixBindingPower(po.Operator.Operator) : null,
                PrefixExpression<T> pr => !relativeToLeft ? operatorProvider.PrefixBindingPower(pr.Operator.Operator) : null,
                _ => null
            };
        }

        public IExpression<T> NameAtom(string name) => expressionFactory.CreateAtomExpression(CreateTermToken(name));

        public IExpression<T> NumberAtom(int i) => expressionFactory.CreateAtomExpression(CreateNumberToken(i));

        public IExpression<T> StringAtom(string str) => expressionFactory.CreateAtomExpression(CreateStringToken(str));

        public OperatorToken Op(string op)
        {
            if (operatorProvider.InfixBindingPower(op) is not null) return new() { Operator = op, LeadingTrivia = " ", TrailingTrivia = " " };
            else return new OperatorToken() { Operator = op }.WithEmptyTrivia();
        }

        private static NameToken CreateTermToken(string name) => new NameToken()
        {
            Value = name,
        }.WithEmptyTrivia();

        private static NumberToken CreateNumberToken(int i) => new NumberToken()
        {
            Value = i,
        }.WithEmptyTrivia();

        private static StringToken CreateStringToken(string str) => new()
        {
            Value = str,
            LeadingTrivia = "`",
            TrailingTrivia = "`",
        };
    }
}

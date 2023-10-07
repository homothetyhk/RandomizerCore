namespace RandomizerCore.StringParsing
{
    public interface IExpression<T>
    {
        int StartChar { get; }
        int EndChar { get; }

        /// <summary>
        /// Speculates possible types for an expression
        /// </summary>
        IEnumerable<T> Evaluate();

        bool Validate(ExpressionValidator<T> validator);

        string Print();
    }

    public record GroupingExpression<T>(StructuralToken OpenParenthesis, IExpression<T> Nested, StructuralToken CloseParenthesis) : IExpression<T>
    {
        public int StartChar => OpenParenthesis.StartCharacter;
        public int EndChar => CloseParenthesis.EndCharacter;

        public IEnumerable<T> Evaluate() => Nested.Evaluate();
        public bool Validate(ExpressionValidator<T> validator) => validator.ExpectAllParallel(
            () => validator.Expect(
                () => OpenParenthesis.TokenType == StructuralToken.Type.OpenParenthesis,
                OpenParenthesis.StartCharacter, OpenParenthesis.EndCharacter,
                "Expected an open parenthesis '('."
            ),
            () => validator.Expect(
                () => CloseParenthesis.TokenType == StructuralToken.Type.CloseParenthesis,
                CloseParenthesis.StartCharacter, CloseParenthesis.EndCharacter,
                "Expected a close parenthesis ')'."
            ),
            () => Nested.Validate(validator)
        );

        public string Print() => OpenParenthesis.Print() + Nested.Print() + CloseParenthesis.Print();
    }

    public abstract record PrefixExpression<T>(OperatorToken Operator, IExpression<T> Operand) : IExpression<T>
    {
        public int StartChar => Operator.StartCharacter;
        public int EndChar => Operand.EndChar;

        public abstract IEnumerable<T> Evaluate();
        public abstract bool Validate(ExpressionValidator<T> validator);

        public string Print() => Operator.Print() + Operand.Print();
    }

    public abstract record PostfixExpression<T>(IExpression<T> Operand, OperatorToken Operator) : IExpression<T>
    {
        public int StartChar => Operand.StartChar;
        public int EndChar => Operator.EndCharacter;

        public abstract IEnumerable<T> Evaluate();
        public abstract bool Validate(ExpressionValidator<T> validator);

        public string Print() => Operand.Print() + Operator.Print();
    }

    public abstract record InfixExpression<T>(IExpression<T> Left, OperatorToken Operator, IExpression<T> Right) : IExpression<T>
    {
        public int StartChar => Left.StartChar;
        public int EndChar => Right.EndChar;

        public abstract IEnumerable<T> Evaluate();
        public abstract bool Validate(ExpressionValidator<T> validator);

        public string Print() => Left.Print() + Operator.Print() + Right.Print();
    }
}

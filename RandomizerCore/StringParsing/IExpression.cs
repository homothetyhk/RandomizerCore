namespace RandomizerCore.StringParsing
{
    /// <summary>
    /// An expression with results constrained by the set T
    /// </summary>
    /// <typeparam name="T">The set of valid types for the expression (usually an enum).</typeparam>
    public interface IExpression<T>
    {
        int StartChar { get; }
        int EndChar { get; }

        /// <summary>
        /// Speculates possible types for an expression. If the result { T.A, T.B } is returned, then the result
        /// is to be interpreted as "A or B".
        /// </summary>
        IEnumerable<T> Evaluate();

        /// <summary>
        /// Validates whether the expression is structurally and semantically valid and aggregates any errors
        /// using the provided expression validator.
        /// </summary>
        /// <param name="validator">The validator to use to aggregate errors.</param>
        /// <returns>Whether the expression is valid or not.</returns>
        bool Validate(ExpressionValidator<T> validator);

        /// <summary>
        /// Reconstructs the original string representation of the expression
        /// </summary>
        string Print();
    }

    /// <summary>
    /// A parenthesized nested expression.
    /// </summary>
    /// <param name="OpenParenthesis">The open parenthesis token</param>
    /// <param name="Nested">The nested expression</param>
    /// <param name="CloseParenthesis">The close parenthesis token</param>
    public record GroupingExpression<T>(StructuralToken OpenParenthesis, IExpression<T> Nested, StructuralToken CloseParenthesis) : IExpression<T>
    {
        public int StartChar => OpenParenthesis.StartCharacter;
        public int EndChar => CloseParenthesis.EndCharacter;

        /// <inheritdoc/>
        public IEnumerable<T> Evaluate() => Nested.Evaluate();

        /// <inheritdoc/>
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

        /// <inheritdoc/>
        public string Print() => OpenParenthesis.Print() + Nested.Print() + CloseParenthesis.Print();
    }

    /// <summary>
    /// A prefix expression (prefix operator applied to a nested expression)
    /// </summary>
    /// <param name="Operator">The operator token</param>
    /// <param name="Operand">The operand expression</param>
    public abstract record PrefixExpression<T>(OperatorToken Operator, IExpression<T> Operand) : IExpression<T>
    {
        public int StartChar => Operator.StartCharacter;
        public int EndChar => Operand.EndChar;

        /// <inheritdoc/>
        public abstract IEnumerable<T> Evaluate();
        /// <inheritdoc/>
        public abstract bool Validate(ExpressionValidator<T> validator);

        /// <inheritdoc/>
        public string Print() => Operator.Print() + Operand.Print();
    }

    /// <summary>
    /// A postfix expression (postfix operator applied to a nested expression)
    /// </summary>
    /// <param name="Operand">The operand expression</param>
    /// <param name="Operator">The operator token</param>
    public abstract record PostfixExpression<T>(IExpression<T> Operand, OperatorToken Operator) : IExpression<T>
    {
        public int StartChar => Operand.StartChar;
        public int EndChar => Operator.EndCharacter;

        /// <inheritdoc/>
        public abstract IEnumerable<T> Evaluate();
        /// <inheritdoc/>
        public abstract bool Validate(ExpressionValidator<T> validator);

        /// <inheritdoc/>
        public string Print() => Operand.Print() + Operator.Print();
    }

    /// <summary>
    /// An infix expression (infix operator applied between 2 nested expressions)
    /// </summary>
    /// <param name="Left">The left-hand operand</param>
    /// <param name="Operator"></param>
    /// <param name="Right">The right-hand operand</param>
    public abstract record InfixExpression<T>(IExpression<T> Left, OperatorToken Operator, IExpression<T> Right) : IExpression<T>
    {
        public int StartChar => Left.StartChar;
        public int EndChar => Right.EndChar;

        /// <inheritdoc/>
        public abstract IEnumerable<T> Evaluate();
        /// <inheritdoc/>
        public abstract bool Validate(ExpressionValidator<T> validator);

        /// <inheritdoc/>
        public string Print() => Left.Print() + Operator.Print() + Right.Print();
    }
}

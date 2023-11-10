namespace RandomizerCore.StringParsing
{
    /// <summary>
    /// Factory to produce domain specific expressions during parsing. Should support operators as defined
    /// by a paired <see cref="IOperatorProvider"/>
    /// </summary>
    public interface IExpressionFactory<T>
    {
        /// <summary>
        /// Determines whether a token is a valid atomic expression
        /// </summary>
        bool IsAtom(Token token);
        /// <summary>
        /// Creates an atom expression from a token
        /// </summary>
        IExpression<T> CreateAtomExpression(Token token);
        /// <summary>
        /// Creates the appropriate prefix expression for the provided operator
        /// </summary>
        IExpression<T> CreatePrefixExpression(OperatorToken op, IExpression<T> operand);
        /// <summary>
        /// Creates the appropriate postfix expression for the provided operator
        /// </summary>
        IExpression<T> CreatePostfixExpression(IExpression<T> operand, OperatorToken op);
        /// <summary>
        /// Creates the appropriate infix expression for the provided operator
        /// </summary>
        IExpression<T> CreateInfixExpression(IExpression<T> left, OperatorToken op, IExpression<T> right);
    }
}

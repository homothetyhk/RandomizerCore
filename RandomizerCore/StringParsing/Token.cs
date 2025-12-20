namespace RandomizerCore.StringParsing
{
    /// <summary>
    /// Record carrying part of the raw data of an expression, organized into operators, groupings (parens), and data (names, numbers, raw text).
    /// </summary>
    /// <param name="Content">The string representation of the token's data. Use <see cref="Print"/> over <see cref="Content"/> when writing tokens within expressions.</param>
    public abstract record Token(string Content)
    {
        /// <summary>
        /// Prints the content of the token, along with any required delimiters.
        /// </summary>
        public virtual string Print() => Content;
    }

    public record NameToken(string Content) : Token(Content);
    public record StringToken(char Delimiter, string Content) : Token(Content)
    {
        public override string Print()
        {
            return Delimiter + Content + Delimiter;
        }
    }
    public record NumberToken(int Value) : Token(Value.ToString());
    public record OperatorToken(OperatorDefinition Definition) : Token(Definition.Operator);
    public record StructuralToken(StructuralToken.Types Type) : Token(Type == Types.OpenParenthesis ? "(" : ")")
    {
        public enum Types
        {
            OpenParenthesis,
            CloseParenthesis
        }
    }
}

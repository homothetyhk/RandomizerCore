namespace RandomizerCore.StringParsing
{
    public abstract record Token
    {
        public int StartCharacter;
        public int EndCharacter;
        public string LeadingTrivia;
        public string TrailingTrivia;

        public string Print() => LeadingTrivia + PrintContent() + TrailingTrivia;
        protected abstract string PrintContent();
    }

    /// <summary>
    /// Token representing a named symbol, e.g. a term, logic variable, item name, etc.
    /// </summary>
    public record NameToken : Token
    {
        public string Value;

        protected override string PrintContent() => Value;
    }

    /// <summary>
    /// Token representing a number literal
    /// </summary>
    public record NumberToken : Token
    {
        public int Value;

        protected override string PrintContent() => Value.ToString();
    }

    /// <summary>
    /// Token representing a string literal
    /// </summary>
    public record StringToken : Token
    {
        public string Value;

        protected override string PrintContent() => Value;
    }

    /// <summary>
    /// Token representing a structural element, e.g. parentheses or braces
    /// </summary>
    public record StructuralToken : Token
    {
        public enum Type
        {
            OpenParenthesis,
            CloseParenthesis
        }
        public Type TokenType;

        protected override string PrintContent() => TokenType switch
        {
            Type.OpenParenthesis => "(",
            Type.CloseParenthesis => ")",
            _ => throw new NotImplementedException()
        };
    }

    /// <summary>
    /// Token representing a unary or binary operator
    /// </summary>
    public record OperatorToken : Token
    {
        public string Operator;

        protected override string PrintContent() => Operator;
    }
}

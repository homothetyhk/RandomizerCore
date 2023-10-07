namespace RandomizerCore.StringParsing
{
    [Serializable]
    public class TokenizingException : Exception
    {
        public TokenizingException() { }
        public TokenizingException(string message) : base(message) { }
        public TokenizingException(string message, Exception inner) : base(message, inner) { }
        protected TokenizingException(
          System.Runtime.Serialization.SerializationInfo info,
          System.Runtime.Serialization.StreamingContext context) : base(info, context) { }
    }

    [Serializable]
    public class ParsingException : Exception
    {
        public ParsingException() { }
        public ParsingException(string message) : base(message) { }
        public ParsingException(string message, Exception inner) : base(message, inner) { }
        protected ParsingException(
          System.Runtime.Serialization.SerializationInfo info,
          System.Runtime.Serialization.StreamingContext context) : base(info, context) { }
    }
}

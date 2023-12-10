namespace RandomizerCore.StringParsing
{
    [Serializable]
    public class TokenizingException : Exception
    {
        public TokenizingException() { }
        public TokenizingException(string message) : base(message) { }
        public TokenizingException(string message, Exception inner) : base(message, inner) { }

#if !NET8_0_OR_GREATER
        protected TokenizingException(
          System.Runtime.Serialization.SerializationInfo info,
          System.Runtime.Serialization.StreamingContext context) : base(info, context) { }
#endif
    }

    [Serializable]
    public class ParsingException : Exception
    {
        public ParsingException() { }
        public ParsingException(string message) : base(message) { }
        public ParsingException(string message, Exception inner) : base(message, inner) { }

#if !NET8_0_OR_GREATER
        protected ParsingException(
          System.Runtime.Serialization.SerializationInfo info,
          System.Runtime.Serialization.StreamingContext context) : base(info, context) { }
#endif
    }
}

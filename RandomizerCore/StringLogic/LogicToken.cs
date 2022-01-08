namespace RandomizerCore.StringLogic
{
    /// <summary>
    /// The fundamental unit of tokenized logic, used throughout the StringLogic namespace.
    /// </summary>
    public abstract record LogicToken;
    
    /// <summary>
    /// LogicToken representing one of the binary boolean operators, | or +.
    /// </summary>
    public record OperatorToken(OperatorType OperatorType, int Precedence, string Symbol) : LogicToken
    {
        public static readonly OperatorToken AND = new(OperatorType.AND, 1, "+");
        public static readonly OperatorToken OR = new(OperatorType.OR, 0, "|");
    }
    public enum OperatorType
    {
        OR,
        AND,
    }

    /// <summary>
    /// LogicToken which evaluates to a bool.
    /// </summary>
    public abstract record TermToken : LogicToken
    {
        public abstract string Write();
        
        public static LogicClause operator|(TermToken t, TermToken u)
        {
            return new(t, u, OperatorToken.OR);
        }
        public static LogicClause operator +(TermToken t, TermToken u)
        {
            return new(t, u, OperatorToken.AND);
        }
    }

    /// <summary>
    /// TermToken which represents a simple named variable.
    /// </summary>
    public record SimpleToken(string Name) : TermToken
    {
        public override string Write() => Name;
    }
    
    /// <summary>
    /// TermToken which represents a simple comparison of two named integer variables.
    /// </summary>
    public record ComparisonToken(ComparisonType ComparisonType, string Left, string Right) : TermToken
    {
        public override string Write()
        {
            char symbol = ComparisonType switch
            {
                ComparisonType.EQ => '=',
                ComparisonType.LT => '<',
                ComparisonType.GT => '>',
                _ => throw new NotImplementedException(),
            };
            return $"{Left}{symbol}{Right}";
        }
    }
    public enum ComparisonType
    {
        EQ,
        LT,
        GT,
    }

    /// <summary>
    /// TermToken which represents a nested LogicClause, provided through the IMacroSource, usually a LogicProcessor.
    /// </summary>
    public record MacroToken(string Name, IMacroSource Source) : TermToken
    {
        public override string Write() => Name;
        public LogicClause Value => Source.GetMacro(Name);
    }

    /// <summary>
    /// TermToken which represents a constant bool.
    /// </summary>
    public record ConstToken(bool Value) : TermToken
    {
        public override string Write() => Value.ToString().ToUpper();
        public static readonly ConstToken True = new(true);
        public static readonly ConstToken False = new(false);
    }
}

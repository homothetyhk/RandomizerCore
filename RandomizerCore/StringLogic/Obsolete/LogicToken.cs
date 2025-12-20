namespace RandomizerCore.StringLogic
{
    /// <summary>
    /// The fundamental unit of tokenized logic, used throughout the StringLogic namespace.
    /// </summary>
    [Obsolete] 
    public abstract record LogicToken;

    /// <summary>
    /// LogicToken representing one of the binary boolean operators, | or +.
    /// </summary>
    [Obsolete]
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
    [Obsolete]
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
    [Obsolete]
    public record SimpleToken(string Name) : TermToken
    {
        public override string Write() => Name;
    }

    /// <summary>
    /// TermToken which represents a simple comparison of two named integer variables.
    /// </summary>
    [Obsolete]
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
    [Obsolete]
    public record MacroToken(string Name, IMacroSource Source) : TermToken
    {
        public override string Write() => Name;
        public LogicClause Value => Source.GetMacro(Name);
    }

    /// <summary>
    /// TermToken which represents a nested LogicClause by name.
    /// </summary>
    [Obsolete]
    public record ReferenceToken(string Target) : TermToken
    {
        public override string Write() => $"*{Target}";
    }

    /// <summary>
    /// TermToken which is parsed as its left argument if defined, otherwise as its right argument.
    /// </summary>
    [Obsolete]
    public record CoalescingToken(TermToken Left, TermToken Right) : TermToken
    {
        public override string Write() => $"{Left.Write()}?{Right.Write()}";
    }

    /// <summary>
    /// TermToken which represents a constant bool.
    /// </summary>
    [Obsolete]
    public record ConstToken(bool Value) : TermToken
    {
        public override string Write() => Value.ToString().ToUpper();
        public static readonly ConstToken True = new(true);
        public static readonly ConstToken False = new(false);
    }

    /// <summary>
    /// TermToken which wraps a state-valued TermToken, indicating that its state should be projected to a bool.
    /// </summary>
    [Obsolete]
    public record ProjectedToken : TermToken
    {
        public TermToken Inner { get; }
        public ProjectedToken(ReferenceToken inner) => Inner = inner;
        public ProjectedToken(SimpleToken inner) => Inner = inner;
        public ProjectedToken(TermToken inner) => Inner = inner;
        public override string Write() => Inner.Write() + '/';
    }

    /// <summary>
    /// TermToken to allow using expressions in certain contexts that previously expected a single token (coalesce expressions, projection expressions).
    /// </summary>
    [Obsolete]
    public record ClauseToken(LogicClause Clause) : TermToken
    {
        public override string Write()
        {
            return Clause.ToInfix();
        }
    }

}

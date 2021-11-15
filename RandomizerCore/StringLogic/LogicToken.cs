using System;

namespace RandomizerCore.StringLogic
{
    public abstract record LogicToken;
    public enum OperatorType
    {
        OR,
        AND,
    }
    public record OperatorToken(OperatorType OperatorType, int Precedence, string Symbol) : LogicToken
    {
        public static readonly OperatorToken AND = new(OperatorType.AND, 1, "+");
        public static readonly OperatorToken OR = new(OperatorType.OR, 0, "|");
    }
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
    public record SimpleToken(string Name) : TermToken
    {
        public override string Write() => Name;
    }
    public enum ComparisonType
    {
        EQ,
        LT,
        GT,
    }
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

    public record MacroToken(string Name, LogicClause Value) : TermToken
    {
        public override string Write() => Name;
    }
    public record ConstToken(bool Value) : TermToken
    {
        public override string Write() => Value.ToString().ToUpper();
        public static readonly ConstToken True = new(true);
        public static readonly ConstToken False = new(false);
    }
}

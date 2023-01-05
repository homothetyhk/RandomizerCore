using System.Collections;
using System.Collections.ObjectModel;

namespace RandomizerCore.StringLogic
{
    public readonly struct NamedLogicClause
    {
        public NamedLogicClause(string Name, LogicClause Clause)
        {
            this.Name = Name;
            this.Clause = Clause;
        }

        public NamedLogicClause(Logic.RawLogicDef def)
        {
            this.Name = def.name;
            this.Clause = new(def.logic);
        }

        public NamedLogicClause(string name, string infix)
        {
            this.Name = name;
            this.Clause = new(infix);
        }

        public static implicit operator LogicClause(NamedLogicClause def) => def.Clause;


        public readonly string Name;
        public readonly LogicClause Clause;
    }

    /// <summary>
    /// An immutable representation of a boolean circuit, consisting of tokens in RPN order.
    /// </summary>
    public class LogicClause : IReadOnlyList<LogicToken>
    {
        public readonly ReadOnlyCollection<LogicToken> Tokens;

        public static readonly LogicClause EmptyDisjunction = new(ConstToken.False);
        public static readonly LogicClause EmptyConjunction = new(ConstToken.True);

        public LogicClause(string infix)
        {
            Tokens = Infix.Tokenize(infix).AsReadOnly();
        }

        public LogicClause(string infix, ITokenSource tokenSource)
        {
            Tokens = Infix.Tokenize(infix, tokenSource).AsReadOnly();
        }

        public LogicClause(TermToken left, TermToken right, OperatorToken op)
        {
            Tokens = new(new LogicToken[] { left, right, op });
        }

        public LogicClause(TermToken t)
        {
            Tokens = new(new LogicToken[] { t });
        }

        public LogicClause(LogicClauseBuilder lcb)
        {
            if (!lcb.Complete) throw new InvalidOperationException("Cannot construct LogicClause from malformatted LogicClauseBuilder.");
            Tokens = new(lcb.Tokens.ToArray());
        }

        private LogicClause(LogicToken[] tokens)
        {
            Tokens = new(tokens);
        }

        public static LogicClause operator |(LogicClause c, LogicClause d)
        {
            LogicToken[] tokens = new LogicToken[c.Tokens.Count + d.Tokens.Count + 1];
            c.Tokens.CopyTo(tokens, 0);
            d.Tokens.CopyTo(tokens, c.Tokens.Count);
            tokens[^1] = OperatorToken.OR;
            return new(tokens);
        }

        public static LogicClause operator |(LogicClause c, TermToken t)
        {
            LogicToken[] tokens = new LogicToken[c.Tokens.Count + 2];
            c.Tokens.CopyTo(tokens, 0);
            tokens[^2] = t;
            tokens[^1] = OperatorToken.OR;
            return new(tokens);
        }

        public static LogicClause operator |(TermToken t, LogicClause c)
        {
            LogicToken[] tokens = new LogicToken[c.Tokens.Count + 2];
            c.Tokens.CopyTo(tokens, 0);
            tokens[^2] = t;
            tokens[^1] = OperatorToken.OR;
            return new(tokens);
        }

        public static LogicClause operator +(LogicClause c, LogicClause d)
        {
            LogicToken[] tokens = new LogicToken[c.Tokens.Count + d.Tokens.Count + 1];
            c.Tokens.CopyTo(tokens, 0);
            d.Tokens.CopyTo(tokens, c.Tokens.Count);
            tokens[^1] = OperatorToken.AND;
            return new(tokens);
        }

        public static LogicClause operator +(LogicClause c, TermToken t)
        {
            LogicToken[] tokens = new LogicToken[c.Tokens.Count + 2];
            c.Tokens.CopyTo(tokens, 0);
            tokens[^2] = t;
            tokens[^1] = OperatorToken.AND;
            return new(tokens);
        }

        public static LogicClause operator +(TermToken t, LogicClause c)
        {
            LogicToken[] tokens = new LogicToken[c.Tokens.Count + 2];
            c.Tokens.CopyTo(tokens, 0);
            tokens[^2] = t;
            tokens[^1] = OperatorToken.AND;
            return new(tokens);
        }

        public string ToInfix()
        {
            return Infix.ToInfix(Tokens);
        }

        public override string ToString()
        {
            return $"{nameof(LogicClause)} {{ {ToInfix()} }}";
        }

        public int Count => Tokens.Count;

        public LogicToken this[int index] => Tokens[index];

        public IEnumerator<LogicToken> GetEnumerator()
        {
            return ((IEnumerable<LogicToken>)Tokens).GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}

using RandomizerCore.StringParsing;
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
    /// A wrapper for an <see cref="Expression{T}"/> of <see cref="LogicExpressionType"/>, used to store source information.
    /// </summary>
    public class LogicClause
#pragma warning disable CS0612 // Type or member is obsolete
        : IReadOnlyList<LogicToken>
#pragma warning restore CS0612 // Type or member is obsolete
    {
        public Expression<LogicExpressionType> Expr { get; }

        private string? _infix = null;

        public LogicClause(Expression<LogicExpressionType> expr)
        {
            Expr = expr;
#pragma warning disable CS0612 // Type or member is obsolete
            Tokens = new(new LazyTokenList(expr));
#pragma warning restore CS0612 // Type or member is obsolete
        }

        public LogicClause(string infix) : this(LogicExpressionUtil.Parse(infix))
        {
            _infix = infix;
        }

        public LogicClause(LogicClauseBuilder lcb) : this(lcb.Expr)
        {
            _infix = lcb.GetCachedInfix();
        }

        [Obsolete]
        public LogicClause(string infix, ITokenSource tokenSource) : this(infix) { }

        [Obsolete]
        public LogicClause(TermToken t) : this(t.ToExpression()) { }

        [Obsolete]
        public LogicClause(TermToken left, TermToken right, OperatorToken op) : this($"{left.Write()} {op.Symbol} {right.Write()}") { }

        [Obsolete]
        public readonly ReadOnlyCollection<LogicToken> Tokens;

        public string ToInfix() => _infix ??= Expr.Print();

        internal string? GetCachedInfix() => _infix;

        public static LogicClause operator|(LogicClause a, LogicClause b)
        {
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
            return new(builder.ApplyInfixOperator(a.Expr, builder.Op(LogicOperatorProvider.OR), b.Expr));
        }

        public static LogicClause operator+(LogicClause a, LogicClause b)
        {
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
            return new(builder.ApplyInfixOperator(a.Expr, builder.Op(LogicOperatorProvider.AND), b.Expr));
        }

        [Obsolete] public int Count => Tokens.Count;

        [Obsolete] public LogicToken this[int index] => Tokens[index];
        [Obsolete] public IEnumerator<LogicToken> GetEnumerator() => Tokens.GetEnumerator();
        [Obsolete] IEnumerator IEnumerable.GetEnumerator() => ((IEnumerable)Tokens).GetEnumerator();

        [Obsolete]
        internal class LazyTokenList(Expression<LogicExpressionType> Expr) : IList<LogicToken>
        {
            private IList<LogicToken> List { get => field ??= [.. Expr.ToTokenSequence()]; set; }

            public void SetExpr(Expression<LogicExpressionType> expr)
            {
                Expr = expr;
                List = null!;
            }

            public LogicToken this[int index] { get => List[index]; set => List[index] = value; }

            public int Count => List.Count;

            public bool IsReadOnly => List.IsReadOnly;

            public void Add(LogicToken item)
            {
                List.Add(item);
            }

            public void Clear()
            {
                List.Clear();
            }

            public bool Contains(LogicToken item)
            {
                return List.Contains(item);
            }

            public void CopyTo(LogicToken[] array, int arrayIndex)
            {
                List.CopyTo(array, arrayIndex);
            }

            public IEnumerator<LogicToken> GetEnumerator()
            {
                return List.GetEnumerator();
            }

            public int IndexOf(LogicToken item)
            {
                return List.IndexOf(item);
            }

            public void Insert(int index, LogicToken item)
            {
                List.Insert(index, item);
            }

            public bool Remove(LogicToken item)
            {
                return List.Remove(item);
            }

            public void RemoveAt(int index)
            {
                List.RemoveAt(index);
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return ((IEnumerable)List).GetEnumerator();
            }
        }
    }
}

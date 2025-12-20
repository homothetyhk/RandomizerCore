using RandomizerCore.StringParsing;
using System.Collections.ObjectModel;

namespace RandomizerCore.StringLogic
{
    public class LogicClauseBuilder
    {
        public Expression<LogicExpressionType> Expr { get; private set; }
        private (Expression<LogicExpressionType>, string) _infixCache;
        [Obsolete] public readonly ReadOnlyCollection<LogicToken> Tokens;
        [Obsolete] private LogicClause.LazyTokenList list;

        public LogicClauseBuilder(Expression<LogicExpressionType> expr)
        {
#pragma warning disable CS0612 // Type or member is obsolete
            Tokens = new(list = new(expr));
#pragma warning restore CS0612 // Type or member is obsolete
            SetExpr(expr);
        }

        public LogicClauseBuilder(string infix) : this(LogicExpressionUtil.Parse(infix))
        {
            _infixCache = (Expr, infix);
        }

        public LogicClauseBuilder(LogicClauseBuilder other) : this(other.Expr)
        {
            _infixCache = other._infixCache;
        }

        public LogicClauseBuilder(LogicClause other) : this(other.Expr)
        {
            if (other.GetCachedInfix() is string infix) _infixCache = (Expr, infix);
        }

        [Obsolete] public LogicClauseBuilder() : this((Expression<LogicExpressionType>)null!) { }
        [Obsolete] public LogicClauseBuilder(IEnumerable<LogicToken> tokens) : this(tokens.ToExpression()) { }
        [Obsolete] public LogicClauseBuilder(TermToken t) : this(t.ToExpression()) { }
        [Obsolete] public LogicClauseBuilder(string infix, ITokenSource tokenSource) : this(Infix.Tokenize(infix, tokenSource)) { }

        public string ToInfix()
        {
            if (ReferenceEquals(_infixCache.Item1, Expr)) return _infixCache.Item2;
            _infixCache = (Expr, Expr.Print());
            return _infixCache.Item2;
        }

        internal string? GetCachedInfix() => ReferenceEquals(_infixCache.Item1, Expr) ? _infixCache.Item2 : null;

        public void SetExpr(Expression<LogicExpressionType> expr)
        {
            if (expr is null)
            {
                throw new ArgumentNullException(nameof(expr));
            }
            Expr = expr;
#pragma warning disable CS0612 // Type or member is obsolete
            list.SetExpr(expr);
#pragma warning restore CS0612 // Type or member is obsolete
        }

        [Obsolete]
        public void AndWith(IEnumerable<LogicToken> tokens) => AndWith(tokens.ToExpression());

        [Obsolete]
        public void AndWith(TermToken t) => AndWith(t.ToExpression());

        [Obsolete]
        public void AndWith(string infix, ITokenSource tokenSource) =>
            AndWith(StringLogic.Infix.Tokenize(infix, tokenSource).ToExpression());

        public void AndWith(LogicClause c) => AndWith(c.Expr);

        public void AndWith(LogicClauseBuilder lcb) => AndWith(lcb.Expr);

        public void AndWith(Expression<LogicExpressionType> expr)
        {
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
            Expr = builder.ApplyInfixOperator(Expr, builder.Op(LogicOperatorProvider.AND), expr);
        }

        public void AndWith(string infix) => AndWith(LogicExpressionUtil.Parse(infix));

        [Obsolete]
        public void OrWith(IEnumerable<LogicToken> tokens) => OrWith(tokens.ToExpression());

        [Obsolete]
        public void OrWith(TermToken t) => OrWith(t.ToExpression());

        [Obsolete]
        public void OrWith(string infix, ITokenSource tokenSource)
            => OrWith(StringLogic.Infix.Tokenize(infix, tokenSource).ToExpression());

        public void OrWith(LogicClause c) => OrWith(c.Expr);

        public void OrWith(LogicClauseBuilder lcb) => OrWith(lcb.Expr);

        public void OrWith(Expression<LogicExpressionType> expr)
        {
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
            Expr = builder.ApplyInfixOperator(Expr, builder.Op(LogicOperatorProvider.OR), expr);
        }

        public void OrWith(string infix) => OrWith(LogicExpressionUtil.Parse(infix));


        [Obsolete]
        public void AndWithLeft(IEnumerable<LogicToken> tokens) => AndWithLeft(tokens.ToExpression());

        [Obsolete]
        public void AndWithLeft(TermToken t) => AndWithLeft(t.ToExpression());

        [Obsolete]
        public void AndWithLeft(string infix, ITokenSource tokenSource) =>
            AndWithLeft(StringLogic.Infix.Tokenize(infix, tokenSource).ToExpression());

        public void AndWithLeft(LogicClause c) => AndWithLeft(c.Expr);

        public void AndWithLeft(LogicClauseBuilder lcb) => AndWithLeft(lcb.Expr);

        public void AndWithLeft(Expression<LogicExpressionType> expr)
        {
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
            Expr = builder.ApplyInfixOperator(expr, builder.Op(LogicOperatorProvider.AND), Expr);
        }

        public void AndWithLeft(string infix) => AndWithLeft(LogicExpressionUtil.Parse(infix));

        [Obsolete]
        public void OrWithLeft(IEnumerable<LogicToken> tokens) => OrWith(tokens.ToExpression());

        [Obsolete]
        public void OrWithLeft(TermToken t) => OrWith(t.ToExpression());

        [Obsolete]
        public void OrWithLeft(string infix, ITokenSource tokenSource)
            => OrWith(StringLogic.Infix.Tokenize(infix, tokenSource).ToExpression());

        public void OrWithLeft(LogicClause c) => OrWith(c.Expr);

        public void OrWithLeft(LogicClauseBuilder lcb) => OrWith(lcb.Expr);

        public void OrWithLeft(Expression<LogicExpressionType> expr)
        {
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
            Expr = builder.ApplyInfixOperator(expr, builder.Op(LogicOperatorProvider.OR), Expr);
        }

        public void OrWithLeft(string infix) => OrWithLeft(LogicExpressionUtil.Parse(infix));

        /// <inheritdoc cref="ExpressionExtensions.Contains{T}(Expression{T}, Expression{T})"/>
        public bool Contains(Expression<LogicExpressionType> subExpr) => Expr.Contains(subExpr);

        [Obsolete]
        public void Subst(TermToken oldToken, TermToken newToken)
            => Subst(oldToken.ToExpression(), newToken.ToExpression());

        [Obsolete]
        public void Subst(TermToken oldToken, LogicClause newClause)
            => Subst(oldToken.ToExpression(), newClause.Expr);

        /// <summary>
        /// Replaces all occurrences of the old expression with the new expression.
        /// </summary>
        /// <remarks>
        /// Exercise caution when using with a nonatomic old expression.
        /// The infix a+b+c is parsed as (a+b)+c, so subst on a+b+c matches
        /// occurrences of (a+b)+c, but not occurrences of a+(b+c).
        /// </remarks>
        public void Subst(Expression<LogicExpressionType> oldExpr, Expression<LogicExpressionType> newExpr)
        {
            Expr = Expr.Subst(oldExpr, newExpr, LogicExpressionUtil.Builder);
        }

        /// <inheritdoc cref="Subst(Expression{LogicExpressionType}, Expression{LogicExpressionType})"/>
        public void Subst(LogicClause oldClause, LogicClause newClause) => Subst(oldClause.Expr, newClause.Expr);

        /// <inheritdoc cref="Subst(Expression{LogicExpressionType}, Expression{LogicExpressionType})"/>
        public void Subst(string oldInfix, string newInfix) => Subst(LogicExpressionUtil.Parse(oldInfix), LogicExpressionUtil.Parse(newInfix));

        /// <inheritdoc cref="Extensions.PartialCoalesce(Expression{LogicExpressionType}, Func{Expression{LogicExpressionType}, bool?})"/>
        public void PartialCoalesce(Func<Expression<LogicExpressionType>, bool?> validator)
        {
            Expr = Expr.PartialCoalesce(validator);
        }

        /// <inheritdoc cref="Extensions.Coalesce(Expression{LogicExpressionType}, Func{Expression{LogicExpressionType}, bool})"/>
        public void Coalesce(Func<Expression<LogicExpressionType>, bool> validator)
        {
            Expr = Expr.Coalesce(validator);
        }

        /// <inheritdoc cref="Extensions.Unfold(Expression{LogicExpressionType}, Func{string, Expression{LogicExpressionType}?}, Func{string, Expression{LogicExpressionType}?})"/>
        public void Unfold(Func<string, Expression<LogicExpressionType>?> macroResolver, Func<string, Expression<LogicExpressionType>?> referenceResolver)
        {
            Expr = Expr.Unfold(macroResolver, referenceResolver);
        }

        [Obsolete]
        public void Append(TermToken tt)
        {
            if (Expr is null) SetExpr(tt.ToExpression());
            else throw new InvalidOperationException();
        }

        [Obsolete]
        public void Append(IEnumerable<LogicToken> lts)
        {
            if (Expr is null) SetExpr(lts.ToExpression());
            else throw new InvalidOperationException();
        }

        [Obsolete]
        public void Append(LogicClause c)
        {
            if (Expr is null) SetExpr(c.Expr);
            else throw new InvalidOperationException();
        }

        [Obsolete]
        public void Append(LogicClauseBuilder lcb)
        {
            if (Expr is null) SetExpr(lcb.Expr);
            else throw new InvalidOperationException();
        }

        [Obsolete]
        public void Append(string infix)
        {
            Append(new LogicClause(infix));
        }
    }
}

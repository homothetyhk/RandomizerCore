using RandomizerCore.Logic;
using RandomizerCore.Logic.StateLogic;
using RandomizerCore.StringLogic;
using RandomizerCore.StringParsing;
using System.Diagnostics.CodeAnalysis;
using OT = RandomizerCore.StringLogic.OperatorToken;

namespace RandomizerCore.StringLogic
{
    public static class Extensions
    {
        public static Expression<LogicExpressionType> ToExpression(this TermValue tv)
        {
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
            if (tv.Value == 1)
            {
                return builder.NameOrNumberAtom(tv.Term.Name);
            }
            else
            {
                return builder.ApplyInfixOperator(builder.NameOrNumberAtom(tv.Term.Name), builder.Op(LogicOperatorProvider.GT), builder.NumberAtom(tv.Value - 1));
            }
        }

        public static Expression<LogicExpressionType> ToExpression(this ILogicVariable lv)
        {
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
            return lv switch
            {
                ComparisonVariable { Left: var l, Right: var r, Op: var op } => 
                    builder.ApplyInfixOperator(l.ToExpression(), builder.Op(op switch 
                    {
                        > 0 => LogicOperatorProvider.GT,
                        0 => LogicOperatorProvider.EQ,
                        < 0 => LogicOperatorProvider.LT,
                    }), r.ToExpression()),
                SAVComparisonVariable { Left: var l, Right: var r, Op: var op } =>
                    builder.ApplyInfixOperator(l.ToExpression(), builder.Op(op switch
                    {
                        > 0 => LogicOperatorProvider.GT,
                        0 => LogicOperatorProvider.EQ,
                        < 0 => LogicOperatorProvider.LT,
                    }), r.ToExpression()),
                LogicDefVariable { logic: LogicDef ld, projected: bool pb, reference: bool rb } => (pb, rb) switch
                {
                    (true, true) => builder.ApplyPostfixOperator(
                                        builder.ApplyPrefixOperator(
                                            builder.Op(LogicOperatorProvider.REF),
                                            builder.NameAtom(ld.Name)),
                                        builder.Op(LogicOperatorProvider.PROJECT)),
                    (false, true) => builder.ApplyPrefixOperator(
                                        builder.Op(LogicOperatorProvider.REF), 
                                        builder.NameAtom(ld.Name)),
                    (true, false) => builder.ApplyPostfixOperator(
                                        ld.ToExpression(),
                                        builder.Op(LogicOperatorProvider.PROJECT)),
                    (false, false) => ld.ToExpression(),
                },
                ProjectedStateProvider { stateProvider: IStateProvider sp } => builder.ApplyPostfixOperator(builder.NameOrNumberAtom(sp.Name), builder.Op(LogicOperatorProvider.PROJECT)),
                LogicStateProvider { logic: StateLogicDef sld } => builder.ApplyPrefixOperator(
                    builder.Op(LogicOperatorProvider.REF), builder.NameOrNumberAtom(sld.Name)),

                _ => builder.NameOrNumberAtom(lv.Name),
            };

        }

        /// <summary>
        /// Evaluates the expression to an identifier string, throwing an error if not possible.
        /// </summary>
        /// <exception cref="ArgumentException">The expression is not an atom, or a coalescing of atoms.</exception>
        public static string ToIdentifier(this Expression<LogicExpressionType> expr, LogicManager lm) => expr switch
        {
            GroupingExpression<LogicExpressionType> { Nested: Expression<LogicExpressionType> nested } => nested.ToIdentifier(lm),
            LogicAtomExpression { Token.Content: string n } => lm.GetMacro(n) is not MacroDef macro ? n : macro.Logic.Expr.ToIdentifier(lm),
            CoalesceExpression q => q.Left.IsDefined(lm) ? q.Left.ToIdentifier(lm) : q.Right.ToIdentifier(lm),
            AndExpression or OrExpression or ProjectionExpression or ReferenceExpression or ComparisonExpression 
                or BoolLiteralExpression or NumberLiteralExpression or NullLiteralExpression
            => throw new ArgumentException($"Expression {expr.Print()} of type {expr.GetType().Name} is not a legal identifier."),
            _ => throw new NotImplementedException(expr.GetType().Name),
        };

        /// <summary>
        /// Evaluates the expression to the identifier of an int-valued term, variable, or literal.
        /// If the identifier is a number or bool literal, returns a null string and the int value of the literal.
        /// Otherwise, returns a string identifier and 0.
        /// </summary>
        internal static (string?, int) ToComparisonOperand(this Expression<LogicExpressionType> expr, LogicManager lm) => expr switch
        {
            GroupingExpression<LogicExpressionType> { Nested: Expression<LogicExpressionType> nested } => nested.ToComparisonOperand(lm),
            BoolLiteralExpression a => a.ConstValue ? (null, 1) : (null, 0),
            NumberLiteralExpression a => (null, a.ConstValue),
            LogicAtomExpression { Token.Content: string n } => lm.GetMacro(n) is not MacroDef macro ? (n, 0) : macro.Logic.Expr.ToComparisonOperand(lm),
            CoalesceExpression q => q.Left.IsDefined(lm) ? q.Left.ToComparisonOperand(lm) : q.Right.ToComparisonOperand(lm),
            AndExpression or OrExpression or ProjectionExpression or ReferenceExpression or ComparisonExpression or NullLiteralExpression 
            => throw new NotSupportedException($"Expression {expr.Print()} of type {expr.GetType().Name} is not legal as a comparison operand."),
            _ => throw new NotImplementedException(expr.GetType().Name),
        };

        /// <summary>
        /// Returns true if all atoms in the expression are appropriately defined in the LogicManager.
        /// <br/>Used to determine behavior of coalescing operators.
        /// </summary>
        public static bool IsDefined(this Expression<LogicExpressionType> expr, LogicManager lm)
        {
            expr = expr.TrimParens();
            switch (expr)
            {
                case LogicAtomExpression { Token.Content: string n }:
                    if (lm.GetMacro(n) is MacroDef macro)
                    {
                        return macro.Logic.Expr.IsDefined(lm);
                    }
                    else
                    {
                        return lm.IsTermOrVariable(n);
                    }

                case BoolLiteralExpression:
                case NumberLiteralExpression:
                    return true;
                case NullLiteralExpression:
                    return false;

                case CoalesceExpression q:
                    return q.Left.IsDefined(lm) || q.Right.IsDefined(lm);

                case AndExpression a:
                    return a.Left.IsDefined(lm) && a.Right.IsDefined(lm);

                case OrExpression o:
                    return o.Left.IsDefined(lm) && o.Right.IsDefined(lm);

                case ComparisonExpression c:
                    c.Left.ToComparisonOperand(lm);

                    string cleft = c.Left.ToIdentifier(lm);
                    string cright = c.Right.ToIdentifier(lm);
                    return lm.IsTermOrVariable(cleft) && lm.IsTermOrVariable(cright);

                case ProjectionExpression p:
                    return p.Operand.IsDefined(lm);

                case ReferenceExpression r:
                    return lm.IsLogicDef(r.Operand.ToIdentifier(lm));

                default: throw new NotImplementedException(expr.GetType().Name);
            }
        }

        /// <summary>
        /// Replaces CoalesceExpressions with their result as determined by the delegate.
        /// If the delegate returns null, the CoalesceExpression is left in place.
        /// Acts recursively on nested coalescing expressions (even if the delegate returns null on an outer expression).
        /// </summary>
        public static Expression<LogicExpressionType> PartialCoalesce(this Expression<LogicExpressionType> expr,
            Func<Expression<LogicExpressionType>, bool?> validator) => 
                expr.Transform((e, _) => 
                    e is CoalesceExpression q && validator(q.Left) is bool qval
                        ? qval
                            ? q.Left.PartialCoalesce(validator)
                            : q.Right.PartialCoalesce(validator)
                        : null,
                LogicExpressionUtil.Builder);

        /// <summary>
        /// Replaces CoalesceExpressions with their result as determined by the delegate.
        /// Acts recursively on nested coalescing expressions.
        /// </summary>
        public static Expression<LogicExpressionType> Coalesce(this Expression<LogicExpressionType> expr,
            Func<Expression<LogicExpressionType>, bool> validator) =>
                expr.Transform((e, _) =>
                    e is CoalesceExpression q
                        ? validator(q.Left)
                            ? q.Left.Coalesce(validator)
                            : q.Right.Coalesce(validator)
                        : null,
                LogicExpressionUtil.Builder);

        /// <summary>
        /// Replaces macros and references with the logic they represent.
        /// Acts recursively on the inserted logic.
        /// </summary>
        /// <param name="expr">The expression to unfold.</param>
        /// <param name="macroResolver">A delegate to fetch the expression of a macro, or null if the string is not a macro identifier.</param>
        /// <param name="referenceResolver">A delegate to fetch the expression of a reference, or null if the string is not a reference identifier.</param>
        /// <exception cref="ArgumentException">The expression contains a reference expression which cannot be resolved.</exception>
        /// <exception cref="Exceptions.ReferenceCycleException">The expression contains a reference/macro cycle.</exception>
        public static Expression<LogicExpressionType> Unfold(this Expression<LogicExpressionType> expr,
            Func<string, Expression<LogicExpressionType>?> macroResolver,
            Func<string, Expression<LogicExpressionType>?> referenceResolver)
        {
            return expr.Unfold(macroResolver, referenceResolver, new());
        }

        internal static Expression<LogicExpressionType> Unfold(this Expression<LogicExpressionType> expr,
            Func<string, Expression<LogicExpressionType>?> macroResolver,
            Func<string, Expression<LogicExpressionType>?> referenceResolver, CycleDetector cd)
        {
            return expr.Transform((e, _) =>
            {
                switch (e)
                {
                    case LogicAtomExpression { Token.Content: string n }:
                        if (macroResolver(n) is Expression<LogicExpressionType> macroExpr)
                        {
                            cd.PushMacro(n);
                            Expression<LogicExpressionType> result = macroExpr.Unfold(macroResolver, referenceResolver, cd);
                            cd.Pop();
                            return result;
                        }
                        break;

                    case ReferenceExpression r:
                        if (!TryGetReferenceIdentifier(r.Operand, referenceResolver, out string reference, out Expression<LogicExpressionType>? refExpr))
                        {
                            throw new ArgumentException($"Unable to resolve identifier in reference expression {e}", nameof(e));
                        }
                        else
                        {
                            cd.PushReference(reference);
                            Expression<LogicExpressionType> result = refExpr.Unfold(macroResolver, referenceResolver, cd);
                            cd.Pop();
                            return result;
                        }
                }
                return null;
            }, LogicExpressionUtil.Builder);
        
            // local helper
            static bool TryGetReferenceIdentifier(
                Expression<LogicExpressionType> expr, 
                Func<string, Expression<LogicExpressionType>?> referenceResolver, 
                out string identifier, 
                [NotNullWhen(true)] out Expression<LogicExpressionType>? refExpr)
            {
                switch (expr.TrimParens())
                {
                    case LogicAtomExpression a:
                        identifier = a.Token.Content;
                        refExpr = referenceResolver(identifier);
                        return refExpr is not null;
                    case CoalesceExpression q:
                        return TryGetReferenceIdentifier(q.Left, referenceResolver, out identifier, out refExpr) 
                            || TryGetReferenceIdentifier(q.Right, referenceResolver, out identifier, out refExpr);
                    default: throw new NotImplementedException(expr.GetType().Name);
                }
            }
        }


        [Obsolete]
        public static Expression<LogicExpressionType> ToExpression(this TermToken tt)
        {
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
            return tt switch
            {
                SimpleToken { Name: string n } => builder.NameOrNumberAtom(n),
                MacroToken { Name: string n } => builder.NameOrNumberAtom(n),
                ConstToken { Value: bool b } => builder.NameAtom(b.ToString().ToUpper()),
                ComparisonToken { Left: string l, Right: string r, ComparisonType: ComparisonType type } =>
                    builder.ApplyInfixOperator(
                        builder.NameOrNumberAtom(l),
                        builder.Op(type switch
                        {
                            ComparisonType.GT => LogicOperatorProvider.GT,
                            ComparisonType.LT => LogicOperatorProvider.LT,
                            ComparisonType.EQ => LogicOperatorProvider.EQ,
                            _ => throw new NotImplementedException(type.ToString())
                        }),
                        builder.NameOrNumberAtom(r)),
                CoalescingToken { Left: TermToken t1, Right: TermToken t2 } =>
                    builder.ApplyInfixOperator(
                        t1.ToExpression(),
                        builder.Op(LogicOperatorProvider.COALESCE),
                        t2.ToExpression()),
                ProjectedToken { Inner: TermToken inner } =>
                    builder.ApplyPostfixOperator(
                        inner.ToExpression(),
                        builder.Op(LogicOperatorProvider.PROJECT)),
                ReferenceToken { Target: string n } =>
                    builder.ApplyPrefixOperator(
                        builder.Op(LogicOperatorProvider.REF),
                        builder.NameOrNumberAtom(n)),
                ClauseToken { Clause: LogicClause lc } => lc.Expr,
                _ => throw new NotImplementedException(tt.GetType().Name),
            };
        }

        [Obsolete]
        public static Expression<LogicExpressionType> ToExpression(this IEnumerable<LogicToken> tokens)
        {
            Stack<Expression<LogicExpressionType>> stack = [];
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;

            foreach (LogicToken token in tokens)
            {
                if (token is TermToken tt) stack.Push(tt.ToExpression());
                else
                {
                    OT op = (OT)token;
                    var argR = stack.Pop();
                    var argL = stack.Pop();
                    stack.Push(
                        builder.ApplyInfixOperator(
                            argL,
                            builder.Op(op.OperatorType switch
                            {
                                OperatorType.AND => LogicOperatorProvider.AND,
                                OperatorType.OR => LogicOperatorProvider.OR,
                                _ => throw new NotImplementedException(),
                            }),
                            argR));
                }
            }
            if (stack.Count != 1) throw new InvalidOperationException("Malformed rpn expression.");
            return stack.Pop();
        }

        /// <summary>
        /// Attempts to convert the expression to a <see cref="TermToken"/>, throwing an error if the conversion fails.
        /// </summary>
        [Obsolete]
        public static TermToken ToTermToken(this Expression<LogicExpressionType> expr)
        {
            switch (expr)
            {
                case LogicAtomExpression a:
                    return new SimpleToken(a.Token.Content);

                case BoolLiteralExpression b:
                    return new ConstToken(b.ConstValue);

                case NumberLiteralExpression n:
                    return new SimpleToken(n.Token.Content);

                case ComparisonExpression c:
                    ComparisonType ctyp = c.Operator.Definition.Operator switch
                    {
                        LogicOperatorProvider.LT => ComparisonType.LT,
                        LogicOperatorProvider.GT => ComparisonType.GT,
                        LogicOperatorProvider.EQ => ComparisonType.EQ,
                        _ => throw new NotImplementedException(c.Operator.Definition.Operator)
                    };
                    string cleft = ((AtomExpression<LogicExpressionType>)c.Left).Token.Content;
                    string cright = ((AtomExpression<LogicExpressionType>)c.Right).Token.Content;
                    return new ComparisonToken(ctyp, cleft, cright);
                case ReferenceExpression r:
                    return new ReferenceToken(((LogicAtomExpression)r.Operand).Token.Content);
                case ProjectionExpression p:
                    return new ProjectedToken(p.Operand.ToTermToken());
                case CoalesceExpression q:
                    return new CoalescingToken(q.Left.ToTermToken(), q.Right.ToTermToken());
                // formerly illegal token expressions
                /*
                case AndExpression:
                case OrExpression:
                    return new ClauseToken(new LogicClause(expr));
                */

                default: throw new NotImplementedException(expr.GetType().Name);
            }
        }

        [Obsolete]
        public static IEnumerable<LogicToken> ToTokenSequence(this Expression<LogicExpressionType> expr)
        {
            switch (expr)
            {
                case GroupingExpression<LogicExpressionType> g: return g.Nested.ToTokenSequence();
                case AtomExpression<LogicExpressionType> atom: return [atom.ToTermToken()];
                case PrefixExpression<LogicExpressionType> pre: return [pre.ToTermToken()];
                case PostfixExpression<LogicExpressionType> post: return [post.ToTermToken()];
                case ComparisonExpression c: return [c.ToTermToken()];
                case AndExpression a: return [.. a.Left.ToTokenSequence(), .. a.Right.ToTokenSequence(), OperatorToken.AND];
                case OrExpression o: return [.. o.Left.ToTokenSequence(), .. o.Right.ToTokenSequence(), OperatorToken.OR];
                default: throw new NotImplementedException();
            }
        }
    }
}

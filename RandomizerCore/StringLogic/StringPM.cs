using RandomizerCore.Logic;
using RandomizerCore.StringParsing;

namespace RandomizerCore.StringLogic
{
    /// <summary>
    /// Base class for an object which can evaluate logic expressions. Has limited functionality compared to <see cref="ProgressionManager"/>.
    /// </summary>
    public abstract class StringPM
    {
        /// <summary>
        /// The logic processor and macro source tied to the PM.
        /// </summary>
        [Obsolete] public readonly LogicProcessor LP = new();

        private readonly Dictionary<string, LogicClause> macros = [];
        private readonly Dictionary<string, LogicClause> parsedLogic = new();
        private readonly CycleDetector cycleDetector = new();

        public bool Evaluate(string infix)
        {
            if (!parsedLogic.TryGetValue(infix, out LogicClause clause))
            {
                parsedLogic.Add(infix, clause = new(infix));
            }
            return Evaluate(clause);
        }

        public bool Evaluate(LogicClause clause)
        {
            if (clause is null)
            {
                throw new ArgumentNullException(nameof(clause));
            }

            try
            {
                return Evaluate(clause.Expr);
            }
            catch (Exception e)
            {
                cycleDetector.Clear();
                throw new ArgumentException($"Error evaluating logic {clause.ToInfix()}", e);
            }
        }

        private bool Evaluate(Expression<LogicExpressionType> expr) => expr switch
        {
            GroupingExpression<LogicExpressionType> { Nested: Expression<LogicExpressionType> nested } => Evaluate(nested),
            LogicAtomExpression { Token.Content: string atom } => GetMacro(atom) is not LogicClause macro ? EvaluateToBool(atom) : MacroRecursor(atom, macro, Evaluate),
            ComparisonExpression c => EvaluateComparison(c),
            BoolLiteralExpression b => b.ConstValue,
            NumberLiteralExpression n => n.ConstValue > 0,
            AndExpression a => Evaluate(a.Left) && Evaluate(a.Right),
            OrExpression o => Evaluate(o.Left) || Evaluate(o.Right),
            CoalesceExpression q => IsDefined(q.Left) ? Evaluate(q.Left) : Evaluate(q.Right),
            ReferenceExpression or ProjectionExpression => throw new NotSupportedException("Reference and projection operators are not supported for StringPM."),
            _ => throw new NotImplementedException(expr.GetType().Name),
        };

        private bool EvaluateComparison(ComparisonExpression c)
        {
            (string?, int) pleft = ToComparisonOperand(c.Left);
            (string?, int) pright = ToComparisonOperand(c.Right);

            int left = pleft.Item1 is not null ? EvaluateToInt(pleft.Item1) : pleft.Item2;
            int right = pright.Item1 is not null ? EvaluateToInt(pright.Item1) : pright.Item2;
            return c.Operator.Definition.Operator switch
            {
                LogicOperatorProvider.GT => left > right,
                LogicOperatorProvider.LT => left < right,
                LogicOperatorProvider.EQ => left == right,
                _ => throw new NotImplementedException(),
            };
        }


        [Obsolete]
        public virtual bool Evaluate(TermToken token) => throw new NotImplementedException();

        /// <summary>
        /// Determines whether a non-literal atom is defined for the purposes of coalescing.
        /// </summary>
        public abstract bool IsDefined(string atom);

        private bool IsDefined(Expression<LogicExpressionType> expr) => expr switch
        {
            GroupingExpression<LogicExpressionType> { Nested: Expression<LogicExpressionType> nested } => IsDefined(nested),
            LogicAtomExpression { Token.Content: string atom } => GetMacro(atom) is not LogicClause macro ? IsDefined(atom) : MacroRecursor(atom, macro, IsDefined),
            BoolLiteralExpression or NumberLiteralExpression => true,
            AndExpression a => IsDefined(a.Left) && IsDefined(a.Right),
            OrExpression o => IsDefined(o.Left) && IsDefined(o.Right),
            ComparisonExpression c => IsDefined(c.Left) && IsDefined(c.Right),
            CoalesceExpression q => IsDefined(q.Left) || IsDefined(q.Right),
            ReferenceExpression or ProjectionExpression => false,
            _ => throw new NotImplementedException(expr.GetType().Name),
        };

        private string ToIdentifier(Expression<LogicExpressionType> expr) => expr switch
        {
            GroupingExpression<LogicExpressionType> { Nested: Expression<LogicExpressionType> nested } => ToIdentifier(nested),
            LogicAtomExpression { Token.Content: string atom } => GetMacro(atom) is not LogicClause macro ? atom : MacroRecursor(atom, macro, ToIdentifier),
            CoalesceExpression q => IsDefined(q.Left) ? ToIdentifier(q.Left) : ToIdentifier(q.Right),
            AndExpression or OrExpression or ComparisonExpression
                or BoolLiteralExpression or NumberLiteralExpression => throw new ArgumentException($"Expression {expr.Print()} of type {expr.GetType().Name} is not a legal identifier."),
            ReferenceExpression or ProjectionExpression => throw new NotSupportedException("Reference and projection operators are not supported for StringPM."),
            _ => throw new NotImplementedException(expr.GetType().Name),
        };

        private (string?, int) ToComparisonOperand(Expression<LogicExpressionType> expr) => expr switch
        {
            GroupingExpression<LogicExpressionType> { Nested: Expression<LogicExpressionType> nested } => ToComparisonOperand(nested),
            BoolLiteralExpression a => a.ConstValue ? (null, 1) : (null, 0),
            NumberLiteralExpression a => (null, a.ConstValue),
            LogicAtomExpression { Token.Content: string n } => GetMacro(n) is not LogicClause macro ? (n, 0) : MacroRecursor(n, macro, ToComparisonOperand),
            CoalesceExpression q => IsDefined(q.Left) ? ToComparisonOperand(q.Left) : ToComparisonOperand(q.Right),
            AndExpression or OrExpression or ProjectionExpression or ReferenceExpression or ComparisonExpression => throw new NotSupportedException($"Expression {expr.Print()} of type {expr.GetType().Name} is not legal as a comparison operand."),
            _ => throw new NotImplementedException(expr.GetType().Name),
        };

        /// <summary>
        /// Evaluates the non-literal atom.
        /// </summary>
        public abstract int EvaluateToInt(string atom);
        public bool EvaluateToBool(string atom) => EvaluateToInt(atom) > 0;

        public void DefineMacro(string name, LogicClause logic)
        {
            macros[name] = logic;
        }

        private LogicClause? GetMacro(string name) => macros.TryGetValue(name, out LogicClause? value) ? value : null;

        // helper to detect cycles when recursing over macros
        private T MacroRecursor<T>(string name, LogicClause lc, Func<Expression<LogicExpressionType>, T> func)
        {
            cycleDetector.PushMacro(name);
            T t = func(lc.Expr);
            cycleDetector.Pop();
            return t;
        }
    }
}
 
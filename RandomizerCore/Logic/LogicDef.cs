using RandomizerCore.StringLogic;
using RandomizerCore.StringParsing;

namespace RandomizerCore.Logic
{

    /// <summary>
    /// Class representing a logic clause, which can be evaluated with a <see cref="ProgressionManager"/>.
    /// </summary>
    public abstract class LogicDef : ILogicDef
    {
        /// <summary>
        /// An identifier for the LogicDef. May not be unique.
        /// </summary>
        public string Name { get; }
        /// <summary>
        /// The string logic from which the LogicDef was constructed.
        /// </summary>
        public string InfixSource { get; }

        public LogicDef(string name, string infixSource)
        {
            Name = name;
            InfixSource = infixSource;
        }

        /// <summary>
        /// Evaluates the logic, for the given progression data input.
        /// </summary>
        public abstract bool CanGet(ProgressionManager pm);
        /// <summary>
        /// The sequence of terms on which the LogicDef depends. Terms not included in the sequence must not affect the value of CanGet.
        /// </summary>
        public abstract IEnumerable<Term> GetTerms();
        
        [Obsolete]
        public virtual IEnumerable<LogicToken> ToTokenSequence() => ToExpression().ToTokenSequence();

        /// <summary>
        /// Converts the LogicDef to an equivalent expression.
        /// </summary>
        public abstract Expression<LogicExpressionType> ToExpression();

        /// <summary>
        /// Creates a LogicClauseBuilder equivalent to the result of <see cref="ToExpression"/>.
        /// </summary>
        public LogicClauseBuilder ToLogicClauseBuilder() => new(ToExpression());

        /// <summary>
        /// Creates a LogicClause equivalent to the result of <see cref="ToExpression"/>.
        /// </summary>
        public LogicClause ToLogicClause() => new(ToLogicClauseBuilder());

        /// <summary>
        /// A string representation of the LogicDef. Must be logically equivalent to InfixSource, but may be normalized or expanded or simplified in other ways.
        /// </summary>
        public virtual string ToInfix() => ToLogicClauseBuilder().ToInfix();

        public override string ToString() => $"{Name}: {InfixSource}";
    }
}

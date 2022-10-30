using Newtonsoft.Json;
using RandomizerCore.StringLogic;

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
        [JsonProperty("Logic")] public string InfixSource { get; }

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
        /// <summary>
        /// Converts the LogicDef to its equivalent sequence of LogicTokens in RPN format.
        /// </summary>
        public abstract IEnumerable<LogicToken> ToTokenSequence();
        /// <summary>
        /// Creates a LogicClauseBuilder equivalent to the LogicDef using <see cref="ToTokenSequence"/>.
        /// </summary>
        public LogicClauseBuilder ToLogicClauseBuilder() => new(ToTokenSequence());
        /// <summary>
        /// Creates a LogicClause equivalent to the LogicDef using <see cref="ToTokenSequence"/>.
        /// </summary>
        public LogicClause ToLogicClause() => new(ToLogicClauseBuilder());
        /// <summary>
        /// The string representation of the LogicDef. Must be logically equivalent to InfixSource, but generally does not contain macros and may be expanded or simplified in other ways.
        /// </summary>
        public virtual string ToInfix() => ToLogicClauseBuilder().ToInfix();
    }
}

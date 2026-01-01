using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// Terms in logic which require external computation. Cast to a derived class to evaluate.
    /// Main subclasses are currently <see cref="LogicInt"/> and <see cref="StateModifier"/>.
    /// </summary>
    public abstract class LogicVariable : ILogicVariable
    {
        /// <summary>
        /// The name of the variable. Should match its usage in logic.
        /// </summary>
        public abstract string Name { get; }
        /// <summary>
        /// The terms which could possibly alter the evaluation of the variable.
        /// </summary>
        /// <returns></returns>
        public abstract IEnumerable<Term> GetTerms();
        public override string ToString() => Name;

        /// <summary>
        /// Return this to implement bool-like behavior in a <see cref="LogicVariable"/> constrained to returning int.
        /// </summary>
        public const int FALSE = 0;
        /// <summary>
        /// Return this to implement bool-like behavior in a <see cref="LogicVariable"/> constrained to returning int.
        /// </summary>
        public const int TRUE = 1;
    }

    public interface ILogicVariable
    {
        public string Name { get; }
        public abstract IEnumerable<Term> GetTerms();
    }

    /// <summary>
    /// A logic variable which represents a domain-dependent comparison operation. Matched when converting logic back to syntax tree.
    /// </summary>
    internal interface IComparisonVariable : ILogicVariable
    {
        public ILogicVariable Left { get; }
        public ILogicVariable Right { get; }
        /// <summary>
        /// Positive for GT, Zero for EQ, Negative for LT.
        /// </summary>
        public int Op { get; }
    }
}

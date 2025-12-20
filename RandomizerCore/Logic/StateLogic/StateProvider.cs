namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A LogicInt which can provide an input state to its logic branch, if not preceded by any state-valued terms.
    /// </summary>
    public abstract class StateProvider : LogicInt, IStateProvider
    {
        /// <summary>
        /// Gets the collection of states which should be fed into the conjunction containing this variable.
        /// </summary>
        public abstract StateUnion? GetInputState(object? sender, ProgressionManager pm);

        public override int GetValue(object? sender, ProgressionManager pm)
        {
            return GetInputState(sender, pm) is not null ? TRUE : FALSE;
        }
    }

    public interface IStateProvider : ILogicVariable
    {
        public abstract StateUnion? GetInputState(object? sender, ProgressionManager pm);
    }

    internal class LogicStateProvider : StateProvider
    {
        internal readonly StateLogicDef logic;

        public override string Name { get; }

        public LogicStateProvider(StateLogicDef logic)
        {
            this.logic = logic;
            this.Name = $"*{logic.Name}";
        }

        public override StateUnion? GetInputState(object? sender, ProgressionManager pm)
        {
            List<State> result = new();
            return logic.EvaluateState(pm, result) ? new(result) : null;
        }

        public override IEnumerable<Term> GetTerms()
        {
            return logic.GetTerms();
        }
    }
}

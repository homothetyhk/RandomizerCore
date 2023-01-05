namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A LogicVariable which produces an int depending on progression data and a state input.
    /// </summary>
    public abstract class StateAccessVariable : LogicVariable
    {
        public abstract int GetValue<T>(object? sender, ProgressionManager pm, T state) where T : IState;
        /// <summary>
        /// Enumerates the state fields which the variable depends on.
        /// </summary>
        public abstract IEnumerable<StateField> GetStateFields();
    }
}

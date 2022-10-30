namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Variable which acts on a state and either transforms it into another single state or rejects it.
    /// </summary>
    public abstract class StateModifyingVariable : StateVariable
    {
        /// <summary>
        /// Modifies the state, producing a single result. Return true if the result is kept, and false if it should be discarded.
        /// </summary>
        public abstract bool ModifyState(object sender, ProgressionManager pm, ref LazyStateBuilder state);
    }
}

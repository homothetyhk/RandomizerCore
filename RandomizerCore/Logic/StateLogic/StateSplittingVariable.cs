namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Variable which acts on a single state and produces zero or more resulting states.
    /// </summary>
    public abstract class StateSplittingVariable : StateVariable
    {
        /// <summary>
        /// Modifies the state, possibly producing multiple alternatives.
        /// <br/>A null return value is treated equivalently to an empty sequence.
        /// </summary>
        public abstract IEnumerable<LazyStateBuilder>? ModifyState(object sender, ProgressionManager pm, LazyStateBuilder state);
    }
}

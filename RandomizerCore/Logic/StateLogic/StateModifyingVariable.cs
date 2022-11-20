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

        /// <summary>
        /// Default behavior returns TRUE if localState is not null, and ModifyState would return true for one of its states, and returns FALSE otherwise.
        /// </summary>
        public override int GetValue(object sender, ProgressionManager pm, StateUnion? localState)
        {
            if (localState is null) return FALSE;
            for (int i = 0; i < localState.Count; i++)
            {
                LazyStateBuilder state = new(localState[i]);
                if (ModifyState(sender, pm, ref state)) return TRUE;
            }
            return FALSE;
        }
    }
}

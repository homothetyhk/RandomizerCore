using System.Runtime.InteropServices.WindowsRuntime;

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

        /// <summary>
        /// Default behavior returns TRUE if localState is not null, and ModifyState would return nonempty for one of its states, and returns FALSE otherwise.
        /// </summary>
        public override int GetValue(object sender, ProgressionManager pm, StateUnion? localState)
        {
            if (localState is null) return FALSE;
            for (int i = 0; i < localState.Count; i++)
            {
                if (ModifyState(sender, pm, new(localState[i])) is IEnumerable<LazyStateBuilder> seq && seq.Any()) return TRUE;
            }
            return FALSE;
        }
    }
}

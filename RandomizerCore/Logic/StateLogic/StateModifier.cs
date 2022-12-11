namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Variable which acts on a state and yields a sequence of result states. Can also produce input-independent state.
    /// </summary>
    public abstract class StateModifier : LogicVariable
    {
        /// <summary>
        /// Modifies the state, producing a sequence of results.
        /// </summary>
        public abstract IEnumerable<LazyStateBuilder> ModifyState(object? sender, ProgressionManager pm, LazyStateBuilder state);

        /// <summary>
        /// Returns a sequence of states produced by the modifier, independent of input state.
        /// <br/>If the modifier succeeds independently of input state, but does not have input-independent output, it should return empty.
        /// <br/>If the modifier does not succeed independently of input state, it should return null. The default implementation is to always return null.
        /// </summary>
        public virtual IEnumerable<LazyStateBuilder>? ProvideState(object? sender, ProgressionManager pm) => null;


        /// <summary>
        /// Performs ModifyState on each element of the list, then ProvideState, and puts the results in the list. Removes the original elements of the list.
        /// </summary>
        public void ModifyAll(object? sender, ProgressionManager pm, List<LazyStateBuilder> states)
        {
            int count = states.Count;
            for (int i = 0; i < count; i++)
            {
                states.AddRange(ModifyState(sender, pm, states[i]));
            }
            states.RemoveRange(0, count);
            if (ProvideState(sender, pm) is IEnumerable<LazyStateBuilder> pStates) states.AddRange(pStates);
        }

    }
}

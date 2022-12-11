namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A LogicInt which can provide an input state to its logic branch, if not preceded by any state-valued terms.
    /// </summary>
    public abstract class StateProvider : LogicInt
    {
        /// <summary>
        /// Gets the collection of states which should be fed into the conjunction containing this variable.
        /// </summary>
        public abstract StateUnion? GetInputState(object? sender, ProgressionManager pm);

        public override int GetValue(object? sender, ProgressionManager pm)
        {
            return TRUE;
        }
    }
}

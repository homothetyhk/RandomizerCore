namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A <see cref="LogicVariable"/> which produces an int value, and may depend on the input state for its logic branch.
    /// </summary>
    public abstract class StateVariable : LogicVariable
    {
        public abstract int GetValue(object sender, ProgressionManager pm, StateUnion? localState);
    }
}

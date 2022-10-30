namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A LogicDef which also supports state calculation.
    /// </summary>
    public abstract class StateLogicDef : LogicDef
    {
        public StateLogicDef(string name, string infixSource) : base(name, infixSource) { }

        /// <summary>
        /// Uses the current progression data to determine under what state(s) the logic is satisfiable.
        /// <br/>The result is a list of possible states, which is not null, but may be empty, and may or may not be reduced.
        /// </summary>
        public abstract List<State> EvaluateState(ProgressionManager pm);
        /// <summary>
        /// Runs EvaluateState, and returns true if any new states are added to the state union.
        /// </summary>
        public bool CheckForUpdatedState(ProgressionManager pm, StateUnion? current, out StateUnion result)
        {
            List<State> newStates = EvaluateState(pm);
            if (current is null)
            {
                result = new(newStates);
                return true;
            }
            else return StateUnion.TryUnion(current, newStates, out result);
        }
    }
}

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

    internal class StateAccessorWrapper : StateModifier
    {
        protected readonly StateAccessVariable Left;
        protected readonly ILogicVariable? Right;
        protected readonly int Op;

        public StateAccessorWrapper(string name, StateAccessVariable left, ILogicVariable? right = null, int op = 1)
        {
            Name = name;
            Left = left;
            Right = right;
            Op = op;
        }

        public override string Name { get; }

        public override IEnumerable<Term> GetTerms()
        {
            foreach (Term t in Left.GetTerms()) yield return t;
            if (Right is not null) foreach (Term t in Right.GetTerms()) yield return t;
        }

        public override IEnumerable<LazyStateBuilder> ModifyState(object? sender, ProgressionManager pm, LazyStateBuilder state)
        {
            int l = Left.GetValue(sender, pm, state);
            if (Right is null)
            {
                if (l > 0) yield return state;
            }
            else
            {
                int r = Right switch
                {
                    Term T => pm.Get(T),
                    LogicInt li => li.GetValue(sender, pm),
                    StateAccessVariable sav => sav.GetValue(sender, pm, state),
                    _ => throw new NotImplementedException()
                };
                if (Op switch
                {
                    < 0 => l < r,
                    > 0 => l > r,
                    _ => l == r,
                })
                {
                    yield return state;
                }
            }
        }
    }

}

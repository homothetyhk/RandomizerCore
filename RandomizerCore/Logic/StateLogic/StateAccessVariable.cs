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

    internal class SAVFromLogicInt : StateAccessVariable
    {
        private readonly ILogicInt Inner;

        public SAVFromLogicInt(ILogicInt inner)
        {
            Inner = inner;
        }

        public override string Name => Inner.Name;

        public override IEnumerable<StateField> GetStateFields()
        {
            return [];
        }

        public override IEnumerable<Term> GetTerms()
        {
            return Inner.GetTerms();
        }

        public override int GetValue<T>(object? sender, ProgressionManager pm, T state)
        {
            return Inner.GetValue(sender, pm);
        }
    }

    internal class StateModifierFromSAV(StateAccessVariable left, StateAccessVariable right, int op) : StateModifier, IComparisonVariable
    {
        public StateAccessVariable Left { get; } = left;
        public StateAccessVariable Right { get; } = right;
        public int Op { get; } = op;
        ILogicVariable IComparisonVariable.Left => Left;
        ILogicVariable IComparisonVariable.Right => Right;

        public override string Name
        {
            get
            {
                string op = Op switch
                {
                    > 0 => ">",
                    < 0 => "<",
                    0 => "="
                };
                return $"{Left.Name}{op}{Right.Name}";
            }
        }


        public override IEnumerable<Term> GetTerms()
        {
            foreach (Term t in Left.GetTerms()) yield return t;
            foreach (Term t in Right.GetTerms()) yield return t;
        }

        public override IEnumerable<LazyStateBuilder> ModifyState(object? sender, ProgressionManager pm, LazyStateBuilder state)
        {
            int l = Left.GetValue(sender, pm, state);
            int r = Right.GetValue(sender, pm, state);
            return Math.Sign(l.CompareTo(r)) == Op ? [state] : [];
        }
    }

}

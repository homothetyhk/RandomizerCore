using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Logic
{
    public class StateUpdateEntry : UpdateEntryBase
    {
        public StateUpdateEntry(Term term, StateLogicDef logic)
        {
            this.term = term;
            this.logic = logic;
            this.stateSetter = new(term);
        }

        public class StateSetter : ILogicItem
        {
            public readonly Term term;
            public StateUnion value;

            public StateSetter(Term term)
            {
                this.term = term;
            }

            public string Name => $"{GetType().Name}: {term.Name}";

            public void AddTo(ProgressionManager pm)
            {
                pm.SetState(term, value);
            }

            public IEnumerable<Term> GetAffectedTerms()
            {
                yield return term;
            }
        }

        public readonly StateLogicDef logic;
        public readonly Term term;
        public readonly StateSetter stateSetter;
        private readonly List<State> stateAccumulator = new();

        public override IEnumerable<Term> GetTerms()
        {
            return logic.GetTerms();
        }

        public override void Update(ProgressionManager pm, int updateTerm)
        {
            StateUnion? state = pm.GetState(term);
            stateAccumulator.Clear();
            if (logic.CheckForUpdatedState(pm, state, stateAccumulator, updateTerm, out StateUnion newState))
            {
                stateSetter.value = newState;
                pm.Add(stateSetter);
            }
        }

        public override void Update(ProgressionManager pm)
        {
            StateUnion? state = pm.GetState(term);
            stateAccumulator.Clear();
            if (logic.CheckForUpdatedState(pm, state, stateAccumulator, out StateUnion newState))
            {
                stateSetter.value = newState;
                pm.Add(stateSetter);
            }
            else if (state is null && logic.CanGet(pm))
            {
                stateSetter.value = pm.lm.StateManager.Empty;
                pm.Add(stateSetter);
            }
        }

        public override string ToString()
        {
            return $"{GetType().Name}: {term.Name}, {logic.InfixSource}";
        }
    }
}

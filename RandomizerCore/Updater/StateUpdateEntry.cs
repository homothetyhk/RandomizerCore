using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Logic
{
    public class StateUpdateEntry : UpdateEntry
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

            public string Name => term.Name;

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

        public override bool CanGet(ProgressionManager pm)
        {
            return logic.CanGet(pm);
        }

        public override IEnumerable<Term> GetTerms()
        {
            return logic.GetTerms();
        }

        public override void OnAdd(ProgressionManager pm)
        {
            StateUnion? state = pm.GetState(term);

            if (logic.CheckForUpdatedState(pm, state, out StateUnion newState))
            {
                stateSetter.value = newState;
                pm.Add(stateSetter);
            }
        }

        public override void OnRemove(ProgressionManager pm)
        {
        }

        public override bool alwaysUpdate => true;
    }
}

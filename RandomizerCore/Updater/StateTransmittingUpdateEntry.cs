using RandomizerCore.Logic.StateLogic;
using RandomizerCore.Logic;

namespace RandomizerCore.Updater
{
    public class StateTransmittingUpdateEntry : UpdateEntry
    {
        public StateTransmittingUpdateEntry(Term source, Term target)
        {
            this.source = source;
            this.target = target;
            this.stateSetter = new(target);
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
        public readonly Term source;
        public readonly Term target;
        public readonly StateSetter stateSetter;

        public override bool CanGet(ProgressionManager pm)
        {
            return true;
        }

        public override IEnumerable<Term> GetTerms()
        {
            yield return source;
        }

        public override void OnAdd(ProgressionManager pm)
        {
            StateUnion? state = pm.GetState(target);

            StateUnion? newState = pm.GetState(source);

            if (newState is null || newState.Count == 0 && state is not null) return;
            if (state is null || state.Count == 0)
            {
                stateSetter.value = newState;
                pm.Add(stateSetter);
            }
            else if (StateUnion.TryUnion(state, newState, out StateUnion result))
            {
                stateSetter.value = result;
                pm.Add(stateSetter);
            }
        }

        public override void OnRemove(ProgressionManager pm)
        {
        }

        public override bool alwaysUpdate => true;
    }
}

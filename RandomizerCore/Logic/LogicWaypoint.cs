using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Logic
{
    public class LogicWaypoint : ILogicDef, ILogicItem
    {
        public LogicWaypoint(Term term, StateLogicDef logic)
        {
            this.term = term;
            this.logic = logic;
        }

        public readonly StateLogicDef logic;
        public readonly Term term;

        public void AddTo(ProgressionManager pm)
        {
            switch (term.Type)
            {
                case TermType.State:
                    if (pm.GetState(term) is null) pm.SetState(term, new(logic.EvaluateState(pm)));
                    break;
                default:
                    pm.Set(term, 1);
                    break;
            }
        }

        public IEnumerable<Term> GetAffectedTerms()
        {
            yield return term;
        }

        public string Name => logic.Name;

        public bool CanGet(ProgressionManager pm)
        {
            return logic.CanGet(pm);
        }

        public IEnumerable<Term> GetTerms()
        {
            return logic.GetTerms();
        }
    }
}

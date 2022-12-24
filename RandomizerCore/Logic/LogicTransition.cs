using Newtonsoft.Json;
using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Logic
{
    public class LogicTransition : ILogicItem, ILogicDef, ILocationDependentItem, ILocationWaypoint
    {
        [JsonConstructor]
        public LogicTransition(StateLogicDef logic, Term term)
        {
            this.logic = logic;
            this.term = term;
        }

        [JsonIgnore]
        public string Name => logic.Name;

        public readonly StateLogicDef logic;
        public readonly Term term;


        public bool CanGet(ProgressionManager pm) => logic.CanGet(pm);
        public IEnumerable<Term> GetTerms() => logic.GetTerms();
        public void AddTo(ProgressionManager pm)
        {
            if (term.Type == TermType.State)
            {
                pm.GiveMinimumState(term);
            }
            else
            {
                pm.Set(term, 1);
            }
        }

        public IEnumerable<Term> GetAffectedTerms()
        {
            yield return term;
        }

        public void Place(ProgressionManager pm, ILogicDef location)
        {
            if (term.Type != TermType.State) return;
            if (location is LogicTransition lt)
            {
                pm.mu.LinkState(lt.term, term);
            }
            else if (location is RandoTransition rt)
            {
                pm.mu.LinkState(rt.lt.term, term);
            }
        }

        public ILogicItem GetReachableEffect()
        {
            return this;
        }
    }
}

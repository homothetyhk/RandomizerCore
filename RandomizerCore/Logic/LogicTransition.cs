using Newtonsoft.Json;

namespace RandomizerCore.Logic
{
    public class LogicTransition : ILogicItem, ILogicDef
    {
        [JsonConstructor]
        public LogicTransition(OptimizedLogicDef logic, Term term)
        {
            this.logic = logic;
            this.term = term;
        }

        [JsonIgnore]
        public string Name => logic.Name;

        public readonly OptimizedLogicDef logic;
        public readonly Term term;


        public bool CanGet(ProgressionManager pm) => logic.CanGet(pm);
        public IEnumerable<Term> GetTerms() => logic.GetTerms();
        void ILogicItem.AddTo(ProgressionManager pm) => pm.Set(term.Id, 1);
        public IEnumerable<Term> GetAffectedTerms()
        {
            yield return term;
        }
    }
}

using System.Collections.Generic;
using Newtonsoft.Json;

namespace RandomizerCore.Logic
{
    public class LogicTransition : ILogicItem, ILogicDef
    {
        [JsonConstructor]
        public LogicTransition(OptimizedLogicDef logic, LogicTransitionData data, Term term)
        {
            this.logic = logic;
            this.data = data;
            this.term = term;
        }

        [JsonIgnore]
        public string Name => logic.Name;

        public readonly OptimizedLogicDef logic;
        public readonly LogicTransitionData data;
        public readonly Term term;


        public bool CanGet(ProgressionManager pm) => logic.CanGet(pm);
        public IEnumerable<Term> GetTerms() => logic.GetTerms();
        void ILogicItem.AddTo(ProgressionManager pm) => pm.Set(term.Id, 1);
        public IEnumerable<Term> GetAffectedTerms()
        {
            yield return term;
        }

        public RawLogicTransition ToRaw()
        {
            return new RawLogicTransition(data.SceneName, data.GateName, logic.ToInfix(), data.OneWayType);
        }
    }
}

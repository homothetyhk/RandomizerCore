using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Logic
{
    public class LogicTransition : ILogicItem, ILogicDef
    {
        public LogicTransition(RawLogicTransition raw, Term term, LogicDef logic)
        {
            this.logic = logic;
            this.sceneName = raw.sceneName;
            this.gateName = raw.gateName;
            this.oneWayType = raw.oneWayType;
            this.term = term;
        }

        [Newtonsoft.Json.JsonConstructor]
        public LogicTransition(LogicDef logic, string sceneName, string gateName, OneWayType oneWayType, Term term)
        {
            this.logic = logic;
            this.sceneName = sceneName;
            this.gateName = gateName;
            this.oneWayType = oneWayType;
            this.term = term;
        }

        [Newtonsoft.Json.JsonIgnore]
        public string Name => logic.Name;

        public readonly LogicDef logic;
        public readonly string sceneName;
        public readonly string gateName;
        public readonly OneWayType oneWayType;
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

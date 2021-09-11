using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Logic
{
    public readonly struct RawLogicTransition
    {
        [Newtonsoft.Json.JsonConstructor]
        public RawLogicTransition(string sceneName, string gateName, string logic, OneWayType oneWayType)
        {
            this.sceneName = sceneName;
            this.gateName = gateName;
            this.logic = logic;
            this.oneWayType = oneWayType;
        }

        public string Name => $"{sceneName}[{gateName}]";
        public readonly string sceneName;
        public readonly string gateName;
        public readonly string logic;
        public readonly OneWayType oneWayType;
    }

    public class LogicTransition : ILogicItem, ILogicDef
    {
        public LogicTransition(RawLogicTransition raw, int termIndex, LogicDef logic)
        {
            this.logic = logic;
            this.sceneName = raw.sceneName;
            this.gateName = raw.gateName;
            this.oneWayType = raw.oneWayType;
            this.termIndex = termIndex;
        }


        public string Name => logic.Name;

        public readonly LogicDef logic;
        public readonly string sceneName;
        public readonly string gateName;
        public readonly OneWayType oneWayType;
        public readonly int termIndex;


        public bool CanGet(ProgressionManager pm) => logic.CanGet(pm);
        public IEnumerable<int> GetTerms() => logic.GetTerms();
        void ILogicItem.AddTo(ProgressionManager pm) => pm.Set(termIndex, 1);
        public IEnumerable<int> GetAffectedTerms()
        {
            yield return termIndex;
        }
    }
}

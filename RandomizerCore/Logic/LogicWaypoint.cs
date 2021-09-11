using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RandomizerCore.Logic
{
    public class LogicWaypoint : ILogicDef, ILogicItem
    {
        public LogicWaypoint(int id, LogicDef logic)
        {
            this.logic = logic;
            this.id = id;
        }

        public readonly LogicDef logic;
        public readonly int id;

        public void AddTo(ProgressionManager pm)
        {
            pm.Set(id, 1);
        }

        public IEnumerable<int> GetAffectedTerms()
        {
            yield return id;
        }

        public string Name => logic.Name;

        public bool CanGet(ProgressionManager pm)
        {
            return logic.CanGet(pm);
        }

        public IEnumerable<int> GetTerms()
        {
            return logic.GetTerms();
        }
    }
}

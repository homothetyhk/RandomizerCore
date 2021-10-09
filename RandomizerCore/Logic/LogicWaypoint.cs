using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RandomizerCore.Logic
{
    public class LogicWaypoint : ILogicDef, ILogicItem
    {
        public LogicWaypoint(Term term, LogicDef logic)
        {
            this.logic = logic;
            this.term = term;
        }

        public readonly LogicDef logic;
        public readonly Term term;

        public void AddTo(ProgressionManager pm)
        {
            pm.Set(term.Id, 1);
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

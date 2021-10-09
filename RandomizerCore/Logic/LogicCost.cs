using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RandomizerCore.Logic
{
    public abstract class LogicCost
    {
        public abstract bool CanGet(ProgressionManager pm);
        public abstract IEnumerable<Term> GetTerms();
    }
}

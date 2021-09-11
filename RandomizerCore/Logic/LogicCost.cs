using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RandomizerCore.Logic
{
    public abstract class LogicCost
    {
        public abstract bool CanGet(ProgressionManager pm);
        public abstract IEnumerable<int> GetTerms();
    }

    public class SimpleCost : LogicCost
    {
        public string term;
        public int index;
        public int threshold;

        public SimpleCost(LogicManager lm, string term, int amount)
        {
            this.term = term;
            index = lm.GetTermIndex(term);
            threshold = amount;
        }

        public override bool CanGet(ProgressionManager pm) => pm.Has(index, threshold);
        public override IEnumerable<int> GetTerms()
        {
            yield return index;
        }

        public override string ToString()
        {
            return $"SimpleCost {{{term} >= {threshold}}}";
        }
    }
}

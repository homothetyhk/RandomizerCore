using RandomizerCore.Logic;
using System;
using System.Collections.Generic;
using System.Text;

namespace RandomizerCore.LogicItems
{
    /// <summary>
    /// An item which sets a term to the maximum of its current value and some constant value
    /// </summary>
    public sealed record MaximizeItem(string Name, TermValue maximum) : LogicItem(Name), IConditionalItem
    {
        public override void AddTo(ProgressionManager pm)
        {
            if (CheckForEffect(pm))
            {
                pm.Set(maximum);
            }
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            yield return maximum.Term;
        }

        public bool CheckForEffect(ProgressionManager pm) => pm.Get(maximum.Term.Id) < maximum.Value;
    }
}

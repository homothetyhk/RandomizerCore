using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public sealed record CappedItem(string Name, TermValue[] Effects, TermValue Cap) : LogicItem(Name)
    {
        public override void AddTo(ProgressionManager pm)
        {
            if (!pm.Has(Cap.Term.Id, Cap.Value))
            {
                for (int i = 0; i < Effects.Length; i++)
                {
                    pm.Incr(Effects[i].Term.Id, Effects[i].Value);
                }
            }
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            return Effects.Select(e => e.Term);
        }
    }
}

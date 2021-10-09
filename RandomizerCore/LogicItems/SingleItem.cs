using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public sealed record SingleItem(string Name, TermValue Effect) : LogicItem(Name), IRemovableItem
    {
        public override void AddTo(ProgressionManager pm)
        {
            pm.Incr(Effect.Term.Id, Effect.Value);
        }

        public void RemoveFrom(ProgressionManager pm)
        {
            pm.Incr(Effect.Term.Id, Effect.Value);
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            yield return Effect.Term;
        }
    }
}

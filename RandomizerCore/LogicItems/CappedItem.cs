using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public sealed record CappedItem(string Name, LogicItemEffect[] Effects, LogicItemEffect Cap) : LogicItem(Name)
    {
        public override void AddTo(ProgressionManager pm)
        {
            if (!pm.Has(Cap.id, Cap.incr))
            {
                for (int i = 0; i < Effects.Length; i++)
                {
                    pm.Incr(Effects[i].id, Effects[i].incr);
                }
            }
        }

        public override IEnumerable<int> GetAffectedTerms()
        {
            return Effects.Select(e => e.id);
        }
    }

    public sealed class CappedItemTemplate : LogicItemTemplate
    {
        public ItemEffect[] effects;
        public ItemEffect cap;

        public override LogicItem ToLogicItem(ILogicManager lm)
        {
            return new CappedItem(name, effects.Select(e => new LogicItemEffect(e, lm)).ToArray(), new LogicItemEffect(cap, lm));
        }

        public override IEnumerable<string> GetItemFlags()
        {
            return effects.Select(e => e.id);
        }
    }
}

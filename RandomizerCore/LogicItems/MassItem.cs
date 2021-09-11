using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public sealed record MassItem(string Name, LogicItemEffect[] Effects) : LogicItem(Name), IRemovableItem
    {
        public override void AddTo(ProgressionManager pm)
        {
            for (int i = 0; i < Effects.Length; i++)
            {
                pm.Incr(Effects[i].id, Effects[i].incr);
            }
        }
        public void RemoveFrom(ProgressionManager pm)
        {
            for (int i = 0; i < Effects.Length; i++)
            {
                pm.Incr(Effects[i].id, -Effects[i].incr);
            }
        }

        public override IEnumerable<int> GetAffectedTerms()
        {
            return Effects.Select(e => e.id);
        }
    }

    public sealed class MassItemTemplate : LogicItemTemplate
    {
        public ItemEffect[] effects;

        public override LogicItem ToLogicItem(ILogicManager lm)
        {
            return new MassItem(name, effects.Select(e => new LogicItemEffect(e, lm)).ToArray());
        }

        public override IEnumerable<string> GetItemFlags()
        {
            return effects.Select(e => e.id);
        }
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public sealed record SingleItem(string Name, LogicItemEffect Effect) : LogicItem(Name), IRemovableItem
    {
        public override void AddTo(ProgressionManager pm)
        {
            pm.Incr(Effect.id, Effect.incr);
        }

        public void RemoveFrom(ProgressionManager pm)
        {
            pm.Incr(Effect.id, -Effect.incr);
        }

        public override IEnumerable<int> GetAffectedTerms()
        {
            yield return Effect.id;
        }
    }

    public sealed class SingleItemTemplate : LogicItemTemplate
    {
        public ItemEffect effect;

        public override LogicItem ToLogicItem(ILogicManager lm)
        {
            return new SingleItem(name, new LogicItemEffect(effect, lm));
        }

        public override IEnumerable<string> GetItemFlags()
        {
            yield return effect.id;
        }
    }
}

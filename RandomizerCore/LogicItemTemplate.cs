using System.Collections.Generic;
using System.Linq;
using RandomizerCore.Logic;
using RandomizerCore.LogicItems;

namespace RandomizerCore
{
    public abstract class LogicItemTemplate
    {
        public string name;

        public static LogicItemTemplate Single(string id, int incr = 1)
        {
            return new SingleItemTemplate
            {
                effect = new ItemEffect(id, incr)
            };
        }

        public static LogicItemTemplate BoolLike(params string[] ids)
        {
            return new CappedItemTemplate
            {
                cap = new ItemEffect(ids[0], 1),
                effects = ids.Select(i => new ItemEffect(i, 1)).ToArray()
            };
        }

        public static LogicItemTemplate FromEffects(params (string, int)[] effects)
        {
            return new MassItemTemplate
            {
                effects = effects.Select(p => new ItemEffect(p.Item1, p.Item2)).ToArray()
            };
        }

        public static LogicItemTemplate FromEffects(params ItemEffect[] effects)
        {
            return new MassItemTemplate
            {
                effects = effects
            };
        }

        public abstract LogicItem ToLogicItem(ILogicManager lm);
        public abstract IEnumerable<string> GetItemFlags();
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;

namespace RandomizerCore
{
    [Obsolete]
    public class PriorityModifierArgs<T>
    {
        public T item;
        public int index;
        public int priority;
        public int total;
        public Random rng;

    }

    [Obsolete]
    public interface IPriorityModifier<T>
    {
        void Modify(T item, int index, int total, Random rng, ref int priority);
    }

    /*
    public class ShopPriorityModifier : IPriorityModifier<RandoLocation>
    {
        bool first = false;

        public void Modify(RandoLocation item, int index, int total, Random rng, ref int priority)
        {
            if (item.multi)
            {
                if (!first) first = true;
                else priority = total;
            }
        }
    }

    public class DupePriorityModifier : IPriorityModifier<LogicItem>
    {
        public void Modify(LogicItem item, int index, int total, Random rng, ref int priority)
        {
            if (item.Name == "Dupe") priority = 0;
        }
    }
    */
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using static RandomizerCore.LogHelper;
using RandomizerCore.Logic;

namespace RandomizerCore
{
    public readonly struct ItemPlacement
    {
        public readonly RandoItem item;
        public readonly RandoLocation location;
        

        public ItemPlacement(RandoItem item, RandoLocation location)
        {
            this.item = item;
            this.location = location;
        }

        public void Deconstruct(out RandoItem item, out RandoLocation location)
        {
            item = this.item;
            location = this.location;
        }
    }

    public class RandoLocation : ILogicDef
    {
        public LogicDef logic;
        public List<LogicCost> costs;
        public int priority;

        public string Name => logic.Name;

        public bool CanGet(ProgressionManager pm)
        {
            if (costs != null)
            {
                if (costs.Any(l => !l.CanGet(pm))) return false;
            }
            return logic.CanGet(pm);
        }

        public IEnumerable<int> GetTerms()
        {
            var query = logic.GetTerms();
            if (costs != null)
            {
                query = query.Concat(costs.SelectMany(c => c.GetTerms()));
            }
            return query;
        }

        public void AddCost(LogicCost cost)
        {
            if (costs == null) costs = new List<LogicCost>();
            costs.Add(cost);
            Log($"Added cost {cost} to location {Name}");
        }

        public RandoLocation Clone()
        {
            RandoLocation rl = MemberwiseClone() as RandoLocation;
            rl.costs = rl.costs?.ToList();
            return rl;
        }

        public IEnumerable<RandoLocation> CloneMany(int count)
        {
            for (int i = 0; i < count; i++) yield return Clone();
        }

        public override string ToString()
        {
            return Name;
        }

    }
}

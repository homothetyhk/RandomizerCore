using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using static RandomizerCore.LogHelper;

namespace RandomizerCore.Logic
{
    public abstract class LogicInt
    {
        /// <summary>
        /// The name of the variable. Should match its usage in logic.
        /// </summary>
        public abstract string Name { get; }
        public abstract int GetValue(object sender, ProgressionManager pm);
        public abstract IEnumerable<int> GetTerms();
        public override string ToString() => Name;
    }

    public class ConstantInt : LogicInt
    {
        public ConstantInt(int value) => this.value = value;

        public readonly int value;

        public override string Name => value.ToString();
        public override int GetValue(object sender, ProgressionManager pm) => value;
        public override IEnumerable<int> GetTerms() => Enumerable.Empty<int>();
    }

    public class NotchCostInt : LogicInt
    {
        // the ids should correspond to the 1-40 charm nums (i.e. 1-indexed)
        public readonly int[] charmIDs;

        public NotchCostInt(params int[] charmIDs)
        {
            this.charmIDs = charmIDs;
            Array.Sort(charmIDs);
            Name = $"$NotchCost[{string.Join(",", charmIDs)}]";
        }

        public override string Name { get; }

        public override int GetValue(object sender, ProgressionManager pm)
        {
            List<int> notchCosts = pm.ctx?.notchCosts;
            if (notchCosts != null && notchCosts.Count >= charmIDs[^1])
            {
                Log($"Evaluated variable {Name} as {charmIDs.Sum(i => notchCosts[i - 1]) - charmIDs.Max(i => notchCosts[i - 1])}");
                return charmIDs.Sum(i => notchCosts[i - 1]) - charmIDs.Max(i => notchCosts[i - 1]);
            }
            else
            {
                Log($"Evaluated variable {Name} as {charmIDs.Sum(i => CharmNotchCosts.GetVanillaCost(i)) - charmIDs.Max(i => CharmNotchCosts.GetVanillaCost(i))}");
                return charmIDs.Sum(i => CharmNotchCosts.GetVanillaCost(i)) - charmIDs.Max(i => CharmNotchCosts.GetVanillaCost(i));
            }
        }

        public override IEnumerable<int> GetTerms() => Enumerable.Empty<int>();
    }
}

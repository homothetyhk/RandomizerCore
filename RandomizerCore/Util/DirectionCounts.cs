using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Exceptions;
using static RandomizerCore.LogHelper;

namespace RandomizerCore
{
    public class DirectionCounts
    {
        public int[] counts;
        public readonly TransitionInitializer ti;

        public DirectionCounts(TransitionInitializer ti)
        {
            this.ti = ti;
            this.counts = new int[ti.totalCounts.Length];
        }
        public DirectionCounts(DirectionCounts orig)
        {
            this.ti = orig.ti;
            this.counts = orig.counts.ToArray();
        }

        public bool HasMatch(RandoTransition t)
        {
            return counts[t.targetDir] > 0;
        }

        public void AddRange(IEnumerable<RandoTransition> added)
        {
            foreach (var t in added) counts[t.dir]++;
        }

        public DirectionCounts Step(IEnumerable<RandoTransition> foundTransitions, IEnumerable<RandoTransition> placedTransitions)
        {
            DirectionCounts dc = new DirectionCounts(this);
            foreach (var t in foundTransitions) dc.counts[t.dir]++;
            foreach (var t in placedTransitions) dc.counts[t.targetDir]--;

            if (dc.counts.Any(c => c < 0))
            {
                Log(dc);
                throw new OutOfLocationsException("Overallocated transitions during step.");
            }

            return dc;
        }

        public override string ToString()
        {
            return ti.PrintCompare(counts);
        }
    }
}

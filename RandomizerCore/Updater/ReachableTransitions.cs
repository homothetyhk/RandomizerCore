using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;
using static RandomizerCore.LogHelper;

namespace RandomizerCore
{
    [Obsolete]
    public class ReachableTransitions
    {
        List<OldRandoTransition> reachable = new();
        List<ReachableTransitionUpdateEntry> entries = new();

        public ReachableTransitions(MainUpdater updater, IEnumerable<OldRandoTransition> openTransitions)
        {
            updater.OnBeginRecalculate += OnRecalculate;
            foreach (var t in openTransitions.Where(t => t.IsSourceTransition))
            {
                var rtue = new ReachableTransitionUpdateEntry(this, t);
                entries.Add(rtue);
                updater.AddEntry(rtue);
            }
        }

        public bool FoundNewTransitions => reachable.Count > 0;

        private void OnRecalculate()
        {
            reachable.Clear();
        }

        public IEnumerable<OldRandoTransition> GetUnreachableSourceTransitions() => entries.Select(e => e.transition)
            .Where(t => (t.reachable == State.None) && (!t.coupled || t.placed == State.None));

        public void Collect(out List<OldRandoTransition> newReachable)
        {
            newReachable = reachable;
            foreach (var t in newReachable)
            {
                if (t.reachable == State.Temporary) t.reachable = State.Permanent;
                else throw new Exception("Mislabeled transition found in reachable!");
                if (t.coupled && t.placed != State.None) throw new Exception("Placed transition found in reachable!");
            }

            reachable = new List<OldRandoTransition>();
        }

        public string PrintCurrent() => $"Current reachable state: {reachable.Count} with {string.Join(", ", reachable.Select(t => t.Name))}";

        class ReachableTransitionUpdateEntry : UpdateEntry
        {
            public readonly OldRandoTransition transition;
            public readonly ReachableTransitions parent;

            public ReachableTransitionUpdateEntry(ReachableTransitions parent, OldRandoTransition transition)
            {
                this.parent = parent;
                this.transition = transition;
            }

            public override bool alwaysUpdate => transition.placed == State.Temporary;

            public override bool CanGet(ProgressionManager pm) => transition.CanGet(pm);

            public override IEnumerable<Term> GetTerms() => ((ILogicDef)transition).GetTerms();

            public override void OnAdd(ProgressionManager pm)
            {
                LogDebug("New reachable transition: " + transition.Name);
                if (transition.reachable == State.None && (!transition.coupled || transition.placed == State.None))
                {
                    parent.reachable.Add(transition);
                    transition.reachable = State.Temporary;
                    pm.Add(transition);
                }
            }

            public override void OnRemove(ProgressionManager pm)
            {
                if (transition.reachable == State.Temporary) transition.reachable = State.None;
            }
        }
    }
}

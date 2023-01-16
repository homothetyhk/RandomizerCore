using RandomizerCore.Logic;

namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Tracker which works with the MainUpdater to observe when locations from its RandomizationGroup become reachable.
    /// </summary>
    public class GroupLocationTracker
    {
        public readonly bool coupled;
        private List<IRandoLocation> reachable;
        private List<GroupLocationTrackerUpdateEntry> entries;

        private readonly Action onFind;
        private void OnFind() { onFind(); }

        private void ClearReachable()
        {
            reachable.Clear();
        }

        private void InitializeReachable()
        {
            reachable = new List<IRandoLocation>();
        }

        public void Collect(out List<IRandoLocation> newReachable)
        {
            newReachable = reachable;
            foreach (IRandoLocation rl in newReachable)
            {
                if (rl.Reachable != TempState.Temporary)
                {
                    throw new InvalidOperationException($"Mislabeled location found in reachable during Collect: {rl}");
                }
                if (coupled && ((IRandoCouple)rl).Placed != TempState.None)
                {
                    throw new InvalidOperationException($"Placed item found in reachable during Collect: {rl}");
                }

                rl.Reachable = TempState.Permanent;
            }

            InitializeReachable();
        }

        public List<IRandoLocation> FindNonreachableLocations()
        {
            List<IRandoLocation> locations = new();
            foreach (GroupLocationTrackerUpdateEntry e in entries)
            {
                if (e.Location.Reachable == TempState.None && (!coupled || ((IRandoCouple)e.Location).Placed == TempState.None))
                {
                    locations.Add(e.Location);
                }
            }
            return locations;
        }

        public GroupLocationTracker(ProgressionManager pm, RandomizationGroup group, Action onFind)
        {
            coupled = group is CoupledRandomizationGroup;
            InitializeReachable();

            pm.AfterEndTemp += OnEndTemp;
            pm.mu.AddEntries(GetEntries(group.Locations));
            this.onFind = onFind;
        }

        private void OnEndTemp(bool b)
        {
            if (!b) ClearReachable();
        }

        private IEnumerable<UpdateEntry> GetEntries(IReadOnlyList<IRandoLocation> locations)
        {
            if (entries == null)
            {
                entries = new();
                for (int i = 0; i < locations.Count; i++)
                {
                    entries.Add(new GroupLocationTrackerUpdateEntry(this, locations[i]));
                }
            }
            return entries;
        }

        private class GroupLocationTrackerUpdateEntry : UpdateEntry
        {
            public GroupLocationTracker Parent;
            public IRandoLocation Location;

            public GroupLocationTrackerUpdateEntry(GroupLocationTracker parent, IRandoLocation location)
            {
                Parent = parent;
                Location = location;
            }

            public override bool alwaysUpdate => Parent.coupled && ((IRandoCouple)Location).Placed == TempState.Temporary;

            public override bool CanGet(ProgressionManager pm)
            {
                return Location.CanGet(pm);
            }

            public override IEnumerable<Term> GetTerms()
            {
                return Location.GetTerms();
            }

            public override void OnAdd(ProgressionManager pm)
            {
                if (Location.Reachable == TempState.None && (!Parent.coupled || ((IRandoCouple)Location).Placed == TempState.None)) // a placed coupled item can no longer have its location used
                {
                    Parent.reachable.Add(Location);
                    Parent.OnFind();
                    Location.Reachable = TempState.Temporary;
                    if (Location is ILocationWaypoint ilw) pm.Add(ilw.GetReachableEffect());
                }
            }

            public override void OnRemove(ProgressionManager pm)
            {
                if (Location.Reachable == TempState.Temporary) Location.Reachable = TempState.None;
            }

            public override string ToString()
            {
                return $"{GetType().Name}: {Location.Name}, obtained: {obtained}";
            }
        }
    }
}

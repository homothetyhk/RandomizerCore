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
                if (rl.Reachable != State.Temporary)
                {
                    throw new InvalidOperationException($"Mislabeled location found in reachable during Collect: {rl}");
                }
                if (coupled && ((IRandoCouple)rl).Placed != State.None)
                {
                    throw new InvalidOperationException($"Placed item found in reachable during Collect: {rl}");
                }

                rl.Reachable = State.Permanent;
            }

            InitializeReachable();
        }

        public List<IRandoLocation> FindNonreachableLocations()
        {
            List<IRandoLocation> locations = new();
            foreach (GroupLocationTrackerUpdateEntry e in entries)
            {
                if (e.Location.Reachable == State.None && (!coupled || ((IRandoCouple)e.Location).Placed == State.None))
                {
                    locations.Add(e.Location);
                }
            }
            return locations;
        }

        public GroupLocationTracker(MainUpdater mu, RandomizationGroup group, Action onFind)
        {
            coupled = group is CoupledRandomizationGroup;
            InitializeReachable();

            mu.OnBeginRecalculate += OnBeginRecalculate;
            mu.AddEntries(GetEntries(group.Locations));
            this.onFind = onFind;
        }

        private void OnBeginRecalculate()
        {
            ClearReachable();
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

            public override bool alwaysUpdate => Parent.coupled && ((IRandoCouple)Location).Placed == State.Temporary;

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
                if (Location.Reachable == State.None && (!Parent.coupled || ((IRandoCouple)Location).Placed == State.None))
                {
                    Parent.reachable.Add(Location);
                    Parent.OnFind();
                    Location.Reachable = State.Temporary;
                    if (Location is ILogicItem i) pm.Add(i);
                }
            }

            public override void OnRemove(ProgressionManager pm)
            {
                if (Location.Reachable == State.Temporary) Location.Reachable = State.None;
            }

            public override string ToString()
            {
                return $"{Location.Name}: {obtained}";
            }
        }
    }
}

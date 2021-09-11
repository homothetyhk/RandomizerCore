using System.Collections.Generic;
using static RandomizerCore.LogHelper;

namespace RandomizerCore.Logic
{
    public class ReachableLocations
    {
        List<RandoLocation> reachable = new();
        public bool FoundNewLocations => reachable.Count != 0;

        public ReachableLocations(MainUpdater up, IEnumerable<RandoLocation> locations)
        {
            up.OnBeginRecalculate += OnRecalculate;
            up.OnEndRecalculuate += OnEndRecalculuate;
            up.OnReset += Reset;
            foreach (var loc in locations)
            {
                up.AddEntry(new ReachableLocationUpdateEntry
                {
                    parent = this,
                    location = loc,
                });
            }
        }

        private void OnEndRecalculuate()
        {
            LogDebug($"Finished recalculate with {reachable.Count} new reachable locations.");
        }

        public void Collect(out List<RandoLocation> newLocations)
        {
            newLocations = reachable;
            reachable = new List<RandoLocation>();
        }

        public void Reset()
        {
            reachable.Clear();
        }

        private void OnRecalculate()
        {
            LogDebug($"Prior to recalculate: {reachable.Count} new reachable locations.");
            reachable.Clear();
        }

        private class ReachableLocationUpdateEntry : UpdateEntry
        {
            public ReachableLocations parent;
            public RandoLocation location;

            public override bool CanGet(ProgressionManager pm)
            {
                //Log($"Testing logic for location {location.Name}... result: {location.CanGet(pm)}");
                return location.CanGet(pm);
            }

            public override IEnumerable<int> GetTerms()
            {
                return location.GetTerms();
            }

            public override void OnAdd(ProgressionManager pm)
            {
                //Log("Adding unlocked reachable location " + location.name);
                parent.reachable.Add(location);
            }

            public override void OnRemove(ProgressionManager pm)
            {
                return;
            }
        }
    }
}

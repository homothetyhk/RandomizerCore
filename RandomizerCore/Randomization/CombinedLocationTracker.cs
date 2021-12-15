using RandomizerCore.Logic;

namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Class which manages GroupLocationTrackers for each RandomizationGroup to detect when new locations become reachable.
    /// </summary>
    public class CombinedLocationTracker
    {
        private readonly GroupLocationTracker[] trackers;

        public bool FoundNew { get; private set; }
        private void OnFind() => FoundNew = true;

        public CombinedLocationTracker(MainUpdater mu, RandomizationGroup[] groups)
        {
            trackers = new GroupLocationTracker[groups.Length];
            for (int i = 0; i < groups.Length; i++)
            {
                trackers[i] = new(mu, groups[i], OnFind);
            }
            mu.OnBeginRecalculate += () => FoundNew = false;
        }

        public void Collect(out List<IRandoLocation>[] newReachable)
        {
            newReachable = new List<IRandoLocation>[trackers.Length];
            for (int i = 0; i < newReachable.Length; i++) trackers[i].Collect(out newReachable[i]);
            FoundNew = false;
        }

        public List<IRandoLocation>[] FindNonreachableLocations()
        {
            List<IRandoLocation>[] locations = new List<IRandoLocation>[trackers.Length];
            for (int i = 0; i < locations.Length; i++) locations[i] = trackers[i].FindNonreachableLocations();
            return locations;
        }
    }
}

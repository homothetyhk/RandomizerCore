namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Base class for determining how items should be placed from a RandomizationStage.
    /// </summary>
    public abstract class PlacementStrategy
    {
        /// <summary>
        /// Places the items of the stage according to the spheres. It is expected that the output array contains a full list of placements for each RandomizationGroup of the stage in sequence.
        /// </summary>
        /// <param name="stage">The current RandomizationStage.</param>
        /// <param name="spheres">The list of spheres of the randomizer. Each sphere array has a sphere for each RandomizationGroup, containing the items which must be placed at that depth, and the locations unlocked as a result.</param>
        /// <param name="placementState">The current state. Temporary indicates that the placement will be eventually overwritten. Permanent indicates that it is final. Otherwise, the placement may or may not be final.</param>
        /// <returns></returns>
        public abstract List<RandoPlacement>[] PlaceItems(RandomizationStage stage, List<Sphere[]> spheres, State placementState);
    }
}

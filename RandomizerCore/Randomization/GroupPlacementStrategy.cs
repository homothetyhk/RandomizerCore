namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Base class for creating placements for a group.
    /// </summary>
    public abstract class GroupPlacementStrategy
    {
        public abstract List<RandoPlacement> PlaceGroup(RandomizationGroup group, IEnumerable<Sphere> spheres, State placementState);
        public abstract List<RandoPlacement> PlaceCoupledGroup(CoupledRandomizationGroup group, IEnumerable<Sphere> spheres, IEnumerable<Sphere> dualSpheres, State placementState);
    }
}

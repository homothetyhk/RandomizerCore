namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Base class for creating placements for a group.
    /// </summary>
    public abstract class GroupPlacementStrategy
    {
        public abstract List<RandoPlacement> PlaceGroup(RandomizationGroup group, Sphere sphere, TempState placementState);
        public abstract List<RandoPlacement> PlaceCoupledGroup(CoupledRandomizationGroup group, Sphere sphere, Sphere dualSphere, TempState placementState);
        /// <summary>
        /// Called before rerandomization, or if the randomizer resets for subsequent attempts.
        /// </summary>
        public virtual void Reset() { }

        /// <summary>
        /// Creates a copy of the strategy that can be used for another group.
        /// </summary>
        public virtual GroupPlacementStrategy Clone() => (GroupPlacementStrategy)MemberwiseClone();
    }
}

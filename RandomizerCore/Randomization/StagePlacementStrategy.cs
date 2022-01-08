namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Base class for stage-level placement strategy. Manages the placement of each group.
    /// </summary>
    public class StagePlacementStrategy
    {
        /// <summary>
        /// Places the items of the stage according to the spheres. It is expected that the output array contains a full list of placements for each RandomizationGroup of the stage in sequence.
        /// </summary>
        /// <param name="stage">The current RandomizationStage.</param>
        /// <param name="spheres">The list of spheres of the randomizer. Each sphere array has a sphere for each RandomizationGroup, containing the items which must be placed at that depth, and the locations unlocked as a result.</param>
        /// <param name="placementState">The current state. Temporary indicates that the placement will be eventually overwritten. Permanent indicates that it is final. Otherwise, the placement may or may not be final.</param>
        /// <returns></returns>
        public virtual List<RandoPlacement>[] PlaceItems(RandomizationStage stage, List<Sphere[]> spheres, State placementState)
        {
            List<RandoPlacement>[] placements = new List<RandoPlacement>[stage.groups.Length];
            for (int i = 0; i < placements.Length; i++)
            {
                RandomizationGroup group = stage.groups[i];
                if (group is not CoupledRandomizationGroup couple)
                {
                    placements[i] = group.Strategy.PlaceGroup(stage.groups[i], spheres.Select(l => l[i]), placementState);
                }
                else
                {
                    int j = Array.IndexOf(stage.groups, couple.Dual);
                    if (j < 0) throw new InvalidOperationException("Dual group not found in same stage.");
                    if (i <= j)
                    {
                        placements[i] = group.Strategy.PlaceCoupledGroup(couple, spheres.Select(l => l[i]), spheres.Select(l => l[j]), placementState);
                        placements[j] = placements[i].Select(p => new RandoPlacement((IRandoCouple)p.Location, (IRandoCouple)p.Item)).ToList();
                    }
                }
            }

            return placements;
        }
    }
}

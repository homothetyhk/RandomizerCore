using RandomizerCore.Collections;
using RandomizerCore.Exceptions;

namespace RandomizerCore.Randomization
{
    /// <summary>
    /// A simple placement strategy. Provides support for standard and coupled groups, along with a field which allows weighting placements according to logical depth.
    /// </summary>
    public class DefaultPlacementStrategy : PlacementStrategy
    {
        public delegate void DepthPriorityTransformHandler(IRandoItem item, IRandoLocation location, int itemDepth, int itemPriorityDepth, int locationDepth, ref int locationPriority);

        /// <summary>
        /// Invoked on the minimum priority locations of each sphere to modify the priority used to select the location for item.
        /// <br/>Item priority depth is the number of spheres such that the average priority of their forced progression items is less than the priority of item.
        /// </summary>
        public DepthPriorityTransformHandler depthPriorityTransform;

        public DefaultPlacementStrategy(DepthPriorityTransformHandler depthPriorityTransform)
        {
            this.depthPriorityTransform = depthPriorityTransform;
        }

        public DefaultPlacementStrategy(int depthPriorityScalingFactor)
        {
            void DPT(IRandoItem item, IRandoLocation location, int itemDepth, int itemPriorityDepth, int locationDepth, ref int locationPriority)
            {
                if (itemPriorityDepth < locationDepth) return;
                locationPriority -= depthPriorityScalingFactor * locationDepth;
            }

            depthPriorityTransform = DPT;
        }


        public override List<RandoPlacement>[] PlaceItems(RandomizationStage stage, List<Sphere[]> spheres, State placementState)
        {
            List<RandoPlacement>[] placements = new List<RandoPlacement>[stage.groups.Length];
            for (int i = 0; i < placements.Length; i++)
            {
                RandomizationGroup group = stage.groups[i];
                if (group is not CoupledRandomizationGroup couple)
                {
                    placements[i] = PlaceGroup(stage.groups[i], i, spheres, placementState);
                }
                else
                {
                    int j = Array.IndexOf(stage.groups, couple.Dual);
                    if (j < 0) throw new InvalidOperationException("Dual group not found in same stage.");
                    if (i <= j)
                    {
                        placements[i] = PlaceCoupledGroup(group, i, j, spheres, placementState);
                        placements[j] = placements[i].Select(p => new RandoPlacement((IRandoCouple)p.Location, (IRandoCouple)p.Item)).ToList();
                    }
                }
            }

            return placements;
        }

        public virtual List<RandoPlacement> PlaceGroup(RandomizationGroup group, int groupIndex, List<Sphere[]> spheres, State placementState)
        {
            List<PriorityQueue<IRandoLocation>> locations = new();
            SortedArrayList<int> meanSphereProgressionPriorities = new();
            List<RandoPlacement> placements = new();

            for (int i = 0; i < spheres.Count; i++)
            {
                Sphere s = spheres[i][groupIndex];

                foreach (IRandoItem ri in s.Items)
                {
                    IRandoLocation rl = SelectNext(s, locations, meanSphereProgressionPriorities, ri, out int priorityDepth, out int locationDepth, out int adjustedPriority);
                    placements.Add(new(ri, rl));
                    if (placementState == State.Permanent)
                    {
                        Log($"Placed {ri.Name} at {rl.Name}." +
                            $"\n    Item priority: {ri.Priority}, Item depth: {s.depth}, Priority depth: {priorityDepth}" +
                            $"\n    Location priority: {rl.Priority}, Location depth: {locationDepth}, Adjusted priority: {adjustedPriority}");
                    }
                }

                if (s.Items.Count > 0)
                {
                    meanSphereProgressionPriorities.Add(s.Items.Sum(r => r.Priority) / s.Items.Count);
                }
                else meanSphereProgressionPriorities.Add(int.MinValue);

                
                locations.Add(new PriorityQueue<IRandoLocation>(s.Locations, r => r.Priority));
            }

            return placements;
        }

        public virtual List<RandoPlacement> PlaceCoupledGroup(RandomizationGroup group, int groupIndex, int dualIndex, List<Sphere[]> spheres, State placementState)
        {
            List<PriorityQueue<IRandoLocation>> locations = new();
            List<PriorityQueue<IRandoLocation>> dualLocations = new();

            SortedArrayList<int> meanSphereProgressionPriorities = new();
            SortedArrayList<int> dualMeanSphereProgressionPriorities = new();

            List<RandoPlacement> placements = new();

            for (int i = 0; i < spheres.Count; i++)
            {
                Sphere s = spheres[i][groupIndex];

                foreach (IRandoItem ri in s.Items)
                {
                    IRandoLocation rl = SelectNext(s, locations, meanSphereProgressionPriorities, ri, out int priorityDepth, out int locationDepth, out int adjustedPriority);
                    placements.Add(new(ri, rl));
                    if (placementState == State.Permanent)
                    {
                        Log($"Placed {ri.Name} at {rl.Name}." +
                            $"\n    Item priority: {ri.Priority}, Item depth: {s.depth}, Priority depth: {priorityDepth}" +
                            $"\n    Location priority: {rl.Priority}, Location depth: {locationDepth}, Adjusted priority: {adjustedPriority}");
                    }
                }

                if (s.Items.Count > 0)
                {
                    meanSphereProgressionPriorities.Add(s.Items.Sum(r => r.Priority) / s.Items.Count);
                }
                else meanSphereProgressionPriorities.Add(int.MinValue);

                locations.Add(new PriorityQueue<IRandoLocation>(s.Locations, r => r.Priority));
            }

            for (int i = 0; i < spheres.Count; i++)
            {
                Sphere sd = spheres[i][dualIndex];

                foreach (IRandoItem ri in sd.Items)
                {
                    IRandoLocation rl = SelectNext(sd, dualLocations, dualMeanSphereProgressionPriorities, ri, out int priorityDepth, out int locationDepth, out int adjustedPriority);
                    placements.Add(new((IRandoCouple)rl, (IRandoCouple)ri));
                    if (placementState == State.Permanent)
                    {
                        Log($"Placed {ri.Name} at {rl.Name}." +
                            $"\n    Item priority: {ri.Priority}, Item depth: {sd.depth}, Priority depth: {priorityDepth}" +
                            $"\n    Location priority: {rl.Priority}, Location depth: {locationDepth}, Adjusted priority: {adjustedPriority}");
                    }
                }

                if (sd.Items.Count > 0)
                {
                    dualMeanSphereProgressionPriorities.Add(sd.Items.Sum(r => r.Priority) / sd.Items.Count);
                }
                else dualMeanSphereProgressionPriorities.Add(int.MinValue);

                dualLocations.Add(new PriorityQueue<IRandoLocation>(sd.Locations, r => r.Priority));
            }

            for (int i = 0; i < locations.Count; i++)
            {
                while (locations[i].TryExtractMin(out _, out IRandoLocation rl))
                {
                    IRandoCouple rc = (IRandoCouple)rl;
                    IRandoCouple dualLoc = (IRandoCouple)SelectNext(spheres[i][dualIndex], dualLocations, dualMeanSphereProgressionPriorities, rc, out int priorityDepth, out int index, out int locationPriority);
                    placements.Add(new(dualLoc, rc));
                }
            }

            if (dualLocations.Any(l => l.Count > 0)) throw new InvalidOperationException($"Failure in PlaceCoupledGroup: dual group {spheres[0][dualIndex].groupLabel} has " +
                $"{dualLocations.Select(l => l.Count).Sum()} locations leftover after group {spheres[0][groupIndex].groupLabel} was exhausted.");

            return placements;
        }

        public virtual IRandoLocation SelectNext(Sphere s, List<PriorityQueue<IRandoLocation>> locations, SortedArrayList<int> meanSphereProgressionPriorities, IRandoItem item, out int priorityDepth, out int index, out int locationPriority)
        {
            priorityDepth = meanSphereProgressionPriorities.CountLE(item.Priority);

            index = -1;
            locationPriority = int.MaxValue;

            for (int j = 0; j < locations.Count; j++)
            {
                if (locations[j].TryPeek(out int priority, out IRandoLocation rl))
                {
                    if (index < -1) index = j;
                    depthPriorityTransform(item, rl, s.depth, priorityDepth, j, ref priority);

                    if (priority < locationPriority)
                    {
                        index = j;
                        locationPriority = priority;
                    }
                }
            }

            if (index < 0) throw new OutOfLocationsException($"SelectNext failed on group {s.groupLabel}.");
            locations[index].ExtractMin(out IRandoLocation location);
            return location;
        }

    }
}

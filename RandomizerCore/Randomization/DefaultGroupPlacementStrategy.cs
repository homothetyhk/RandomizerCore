using RandomizerCore.Collections;
using RandomizerCore.Exceptions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Randomization
{
    /// <summary>
    /// A simple placement strategy. Provides support for standard and coupled groups, along with a field which allows weighting placements according to logical depth.
    /// </summary>
    public class DefaultGroupPlacementStrategy : GroupPlacementStrategy
    {
        public delegate void DepthPriorityTransformHandler(IRandoItem item, IRandoLocation location, int itemDepth, int itemPriorityDepth, int locationDepth, ref float locationPriority);

        /// <summary>
        /// Invoked on the minimum priority locations of each sphere to modify the priority used to select the location for item.
        /// <br/>Item priority depth is the number of spheres such that the average priority of their forced progression items is less than the priority of item.
        /// </summary>
        public DepthPriorityTransformHandler depthPriorityTransform;

        private readonly List<Func<IRandoItem, IRandoLocation, bool>> _constraints = new();
        /// <summary>
        /// If any of the subscribers to this event return false, then the placement will be rejected unless no alternatives exist.
        /// </summary>
        public event Func<IRandoItem, IRandoLocation, bool> Constraints
        {
            add => _constraints.Add(value);
            remove => _constraints.Remove(value);
        }
        protected bool CanPlace(IRandoItem item, IRandoLocation location)
        {
            foreach (var test in _constraints) if (!test(item, location)) return false;
            return true;
        }

        /// <summary>
        /// Event for when no reachable locations satisfy the constraint for item.
        /// <br/>Raise OutOfLocationsException to trigger rerandomization. Raise other exceptions to halt randomization.
        /// </summary>
        public event Action<IRandoItem, IRandoLocation> OnConstraintViolated;
        protected void InvokeOnConstraintViolated(IRandoItem item, IRandoLocation location)
        {
            OnConstraintViolated?.Invoke(item, location);
        }

        public DefaultGroupPlacementStrategy(DepthPriorityTransformHandler depthPriorityTransform)
        {
            this.depthPriorityTransform = depthPriorityTransform;
        }

        public DefaultGroupPlacementStrategy(float depthPriorityScalingFactor)
        {
            void DPT(IRandoItem item, IRandoLocation location, int itemDepth, int itemPriorityDepth, int locationDepth, ref float locationPriority)
            {
                if (itemPriorityDepth < locationDepth) return;
                locationPriority -= depthPriorityScalingFactor * locationDepth;
            }

            depthPriorityTransform = DPT;
        }

        public override List<RandoPlacement> PlaceGroup(RandomizationGroup group, IEnumerable<Sphere> spheres, State placementState)
        {
            List<SortedArrayList<IRandoLocation>> locations = new();
            SortedArrayList<float> meanSphereProgressionPriorities = new();
            List<RandoPlacement> placements = new();

            foreach (Sphere s in spheres)
            {
                foreach (IRandoItem ri in s.Items)
                {
                    IRandoLocation rl = SelectNext(s, locations, meanSphereProgressionPriorities, ri, out int priorityDepth, out int locationDepth, out float adjustedPriority);
                    placements.Add(new(ri, rl));
                }

                if (s.Items.Count > 0)
                {
                    meanSphereProgressionPriorities.Add(s.Items.Sum(r => r.Priority) / s.Items.Count);
                }
                else meanSphereProgressionPriorities.Add(int.MinValue);

                locations.Add(new SortedArrayList<IRandoLocation>(s.Locations));
            }

            return placements;
        }

        public sealed override List<RandoPlacement> PlaceCoupledGroup(CoupledRandomizationGroup group, IEnumerable<Sphere> spheres, IEnumerable<Sphere> dualSpheres, State placementState)
        {
            return PlaceCoupledGroup(group, spheres.ToList(), dualSpheres.ToList(), placementState);
        }

        public virtual List<RandoPlacement> PlaceCoupledGroup(CoupledRandomizationGroup group, List<Sphere> spheres, List<Sphere> dualSpheres, State placementState)
        {
            List<SortedArrayList<IRandoLocation>> locations = new();
            List<SortedArrayList<IRandoLocation>> dualLocations = new();

            SortedArrayList<float> meanSphereProgressionPriorities = new();
            SortedArrayList<float> dualMeanSphereProgressionPriorities = new();

            List<RandoPlacement> placements = new();

            foreach (Sphere s in spheres)
            {
                foreach (IRandoItem ri in s.Items)
                {
                    IRandoLocation rl = SelectNext(s, locations, meanSphereProgressionPriorities, ri, out int priorityDepth, out int locationDepth, out float adjustedPriority);
                    placements.Add(new(ri, rl));
                }

                if (s.Items.Count > 0)
                {
                    meanSphereProgressionPriorities.Add(s.Items.Sum(r => r.Priority) / s.Items.Count);
                }
                else meanSphereProgressionPriorities.Add(int.MinValue);

                locations.Add(new SortedArrayList<IRandoLocation>(s.Locations));
            }

            foreach (Sphere sd in dualSpheres)
            {
                foreach (IRandoItem ri in sd.Items)
                {
                    IRandoLocation rl = SelectNext(sd, dualLocations, dualMeanSphereProgressionPriorities, ri, out int priorityDepth, out int locationDepth, out float adjustedPriority);
                    placements.Add(new((IRandoCouple)rl, (IRandoCouple)ri));
                }

                if (sd.Items.Count > 0)
                {
                    dualMeanSphereProgressionPriorities.Add(sd.Items.Sum(r => r.Priority) / sd.Items.Count);
                }
                else dualMeanSphereProgressionPriorities.Add(int.MinValue);

                dualLocations.Add(new SortedArrayList<IRandoLocation>(sd.Locations));
            }

            Stack<IRandoLocation> remaining = new();
            for (int i = 0; i < locations.Count; i++)
            {
                foreach (IRandoLocation rl in locations[i])
                {
                    IRandoCouple rc = (IRandoCouple)rl;
                    IRandoCouple dualLoc = (IRandoCouple)SelectNext(dualSpheres[i], dualLocations, dualMeanSphereProgressionPriorities, rc, out int priorityDepth, out int index, out float locationPriority);
                    placements.Add(new(dualLoc, rc));
                }
            }

            if (dualLocations.Any(l => l.Count > 0)) throw new InvalidOperationException($"Failure in PlaceCoupledGroup: dual group {dualSpheres[0].groupLabel} has " +
                $"{dualLocations.Select(l => l.Count).Sum()} locations leftover after group {spheres[0].groupLabel} was exhausted.");

            return placements;
        }

        public IRandoLocation SelectNext(Sphere s, List<SortedArrayList<IRandoLocation>> locations, SortedArrayList<float> meanSphereProgressionPriorities, IRandoItem item, out int priorityDepth, out int depth, out float locationPriority)
        {
            priorityDepth = meanSphereProgressionPriorities.CountLE(item.Priority);

            depth = -1;
            int index = -1;
            locationPriority = float.MaxValue;

            bool constraintSatisfied = false;

            for (int j = 0; j < locations.Count; j++)
            {
                for (int k = 0; k < locations[j].Count; k++)
                {
                    IRandoLocation rl = locations[j][k];
                    if (index < 0)
                    {
                        depth = j;
                        index = k;
                        locationPriority = rl.Priority;
                        constraintSatisfied = CanPlace(item, rl);
                        if (constraintSatisfied) break;
                        else continue;
                    }

                    bool test = CanPlace(item, rl);
                    if (constraintSatisfied && !test) continue;

                    float priority = rl.Priority;
                    depthPriorityTransform(item, rl, s.depth, priorityDepth, j, ref priority);

                    if (priority < locationPriority)
                    {
                        depth = j;
                        index = k;
                        locationPriority = priority;
                        if (test) constraintSatisfied = true;
                    }

                    if (constraintSatisfied) break;
                }
            }

            if (index < 0) throw new OutOfLocationsException($"SelectNext failed on group {s.groupLabel}.");
            IRandoLocation location = locations[depth][index];
            if (!CanPlace(item, location)) InvokeOnConstraintViolated(item, location);
            locations[depth].RemoveAt(index);
            return location;
        }
    }
}

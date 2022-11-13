using RandomizerCore.Collections;
using RandomizerCore.Exceptions;

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

        private readonly List<SortedArrayList<IRandoLocation>> _locations = new();
        private readonly List<SortedArrayList<IRandoLocation>> _dualLocations = new();
        private readonly SortedArrayList<float> _meanSphereProgressionPriorities = new();
        private readonly SortedArrayList<float> _dualMeanSphereProgressionPriorities = new();
        private readonly List<RandoPlacement> _placements = new();

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
            depthPriorityTransform = PriorityTransformUtil.CreateTransform(depthPriorityScalingFactor);
        }

        public override List<RandoPlacement> PlaceGroup(RandomizationGroup group, Sphere sphere, TempState placementState)
        {
            _placements.Clear();

            Log();
            Log($"===Beginning placements for group {group.Label} sphere {sphere.depth}===");
            foreach (IRandoItem ri in sphere.Items)
            {
                IRandoLocation rl = SelectNext(sphere, _locations, _meanSphereProgressionPriorities, ri, out int priorityDepth, out int locationDepth, out float adjustedPriority);
                _placements.Add(new(ri, rl));
                Log($"Placed {ri.Name} at {rl.Name}. Location had original priority {rl.Priority}, depth {locationDepth}. Item has depth {ri.Sphere}, priority {ri.Priority}, priority depth {priorityDepth}. Adjusted location priority was {adjustedPriority}.");
            }

            float meanPriority = sphere.Items.Count > 0 ? sphere.Items.Sum(r => r.Priority) / sphere.Items.Count : float.NaN;
            _meanSphereProgressionPriorities.Add(meanPriority);
            Log($"===Finished placements for group {group.Label} sphere {sphere.depth}. Mean item priority was {meanPriority}===");
            Log();
                
            _locations.Add(new SortedArrayList<IRandoLocation>(sphere.Locations, ComparerUtil.LocationComparer, ComparerUtil.LocationEqualityComparer));

            return _placements;
        }

        public override List<RandoPlacement> PlaceCoupledGroup(CoupledRandomizationGroup group, Sphere sphere, Sphere dualSphere, TempState placementState)
        {
            _placements.Clear();
            bool selfDual = ReferenceEquals(group, group.Dual);

            foreach (IRandoItem ri in sphere.Items)
            {
                IRandoLocation rl = SelectNext(sphere, _locations, _meanSphereProgressionPriorities, ri, out int priorityDepth, out int locationDepth, out float adjustedPriority);
                _placements.Add(new(ri, rl));
            }

            if (sphere.Items.Count > 0)
            {
                _meanSphereProgressionPriorities.Add(sphere.Items.Sum(r => r.Priority) / sphere.Items.Count);
            }
            else _meanSphereProgressionPriorities.Add(int.MinValue);

            _locations.Add(new SortedArrayList<IRandoLocation>(sphere.Locations, ComparerUtil.LocationComparer, ComparerUtil.LocationEqualityComparer));

            if (!selfDual)
            {
                foreach (IRandoItem ri in dualSphere.Items)
                {
                    IRandoLocation rl = SelectNext(dualSphere, _dualLocations, _dualMeanSphereProgressionPriorities, ri, out int priorityDepth, out int locationDepth, out float adjustedPriority);
                    _placements.Add(new((IRandoCouple)rl, (IRandoCouple)ri));
                }

                if (dualSphere.Items.Count > 0)
                {
                    _dualMeanSphereProgressionPriorities.Add(dualSphere.Items.Sum(r => r.Priority) / dualSphere.Items.Count);
                }
                else _dualMeanSphereProgressionPriorities.Add(int.MinValue);

                _dualLocations.Add(new SortedArrayList<IRandoLocation>(dualSphere.Locations, ComparerUtil.LocationComparer, ComparerUtil.LocationEqualityComparer));
            }

            if (sphere.final)
            {
                if (!selfDual)
                {
                    SortedArrayList<IRandoItem> remainingItems = new(_dualLocations.SelectMany(l => l.Cast<IRandoItem>()), ComparerUtil.ItemComparer, ComparerUtil.ItemEqualityComparer);
                    while (remainingItems.TryExtractMin(out IRandoItem ri))
                    {
                        IRandoLocation rl = SelectNext(sphere, _locations, _meanSphereProgressionPriorities, ri, out int priorityDepth, out int locationDepth, out float adjustedPriority);
                        _placements.Add(new(ri, rl));
                    }

                    if (_locations.Any(l => l.Count > 0)) throw new InvalidOperationException($"Failure in PlaceCoupledGroup: group {sphere.groupLabel} has " +
                        $"{_locations.Select(l => l.Count).Sum()} locations leftover after group {dualSphere.groupLabel} was exhausted.");
                }
                else
                {
                    Dictionary<IRandoCouple, int> locDepthLookup = _locations.SelectMany((l, i) => l.Select(rl => (rl, i))).ToDictionary(p => (IRandoCouple)p.rl, p => p.i);
                    SortedArrayList<IRandoItem> remainingItems = new(locDepthLookup.Keys, ComparerUtil.ItemComparer, ComparerUtil.ItemEqualityComparer);
                    while (remainingItems.TryExtractMin(out IRandoItem ri))
                    {
                        IRandoLocation rl = SelectNext(sphere, _locations, _meanSphereProgressionPriorities, ri, out int priorityDepth, out int locationDepth, out float adjustedPriority);
                        _placements.Add(new(ri, rl));
                        remainingItems.Remove((IRandoCouple)rl);
                        _locations[locDepthLookup[(IRandoCouple)ri]].Remove((IRandoCouple)ri);
                    }

                    if (_locations.Any(l => l.Count > 0)) throw new InvalidOperationException($"Failure in PlaceCoupledGroup: group {sphere.groupLabel} has " +
                        $"{_locations.Select(l => l.Count).Sum()} locations leftover after group {dualSphere.groupLabel} was exhausted.");
                }
            }

            return _placements;
        }

        public IRandoLocation SelectNext(Sphere s, List<SortedArrayList<IRandoLocation>> locations, SortedArrayList<float> meanSphereProgressionPriorities, IRandoItem item, out int itemPriorityDepth, out int locationDepth, out float locationPriority)
        {
            itemPriorityDepth = meanSphereProgressionPriorities.CountLE(item.Priority);

            int locationGroupIndex = -1;
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
                        locationGroupIndex = j;
                        index = k;
                        locationPriority = rl.Priority;
                        constraintSatisfied = CanPlace(item, rl);
                        if (constraintSatisfied) break;
                        else continue;
                    }

                    bool test = CanPlace(item, rl);
                    if (constraintSatisfied && !test) continue; // old passes constraint, new fails
                    else if (!(constraintSatisfied ^ test)) // both pass or both fail constraint
                    {
                        float priority = rl.Priority;
                        depthPriorityTransform(item: item, location: rl, itemDepth: s.depth, itemPriorityDepth: itemPriorityDepth, locationDepth: rl.Sphere, locationPriority: ref priority);
                        if (priority >= locationPriority) continue;
                        else locationPriority = priority;
                    }
                    else // old fails constraint, new passes
                    {
                        locationPriority = rl.Priority;
                        depthPriorityTransform(item: item, location: rl, itemDepth: s.depth, itemPriorityDepth: itemPriorityDepth, locationDepth: rl.Sphere, locationPriority: ref locationPriority);
                    }

                    locationGroupIndex = j;
                    index = k;
                    constraintSatisfied = test;

                    if (constraintSatisfied) break;
                }
            }

            if (index < 0) throw new OutOfLocationsException($"SelectNext failed on group {s.groupLabel}.");
            IRandoLocation location = locations[locationGroupIndex][index];
            if (!constraintSatisfied)
            {
                InvokeOnConstraintViolated(item, location);
            }

            locations[locationGroupIndex].RemoveAt(index);
            locationDepth = location.Sphere;
            return location;
        }

        public override void Reset()
        {
            base.Reset();
            _locations.Clear();
            _dualLocations.Clear();
            _meanSphereProgressionPriorities.Clear();
            _dualMeanSphereProgressionPriorities.Clear();
            _placements.Clear();
        }
    }
}

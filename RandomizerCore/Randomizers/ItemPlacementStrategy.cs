using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Collections;
using RandomizerCore.Exceptions;

namespace RandomizerCore.Randomizers
{
    [Obsolete]
    public abstract class ItemPlacementStrategy
    {
        public abstract List<ItemPlacement> Export(IList<ItemSphere> spheres);
    }

    [Obsolete]
    public class DefaultItemPlacementStrategy : ItemPlacementStrategy
    {
        /// <summary>
        /// Inputs: Current randomized priority and location. Output: new location priority. Ignored if null.
        /// </summary>
        public Func<int, RandoLocation, int> depthPriorityTransform = null;

        /// <summary>
        /// If not null, invoked with the current depth and new placements.
        /// </summary>
        public Action<ItemSphere, List<ItemPlacement>> placementRecorder = null;

        public override List<ItemPlacement> Export(IList<ItemSphere> spheres)
        {
            List<ItemPlacement> placements = new();
            PriorityQueue<RandoLocation> queue = new();
            for (int i = 0; i < spheres.Count; i++)
            {
                ItemSphere sphere = spheres[i];
                foreach (RandoLocation location in sphere.locations)
                {
                    if (depthPriorityTransform != null)
                    {
                        location.Priority = depthPriorityTransform(i, location);
                    }

                    queue.Enqueue(location.Priority, location);
                }

                foreach (RandoItem item in sphere.items)
                {
                    if (!queue.TryExtractMin(out int priority, out RandoLocation location))
                    {
                        throw new OutOfLocationsException("Ran out of locations during placement.");
                    }

                    placements.Add(new ItemPlacement(item, location));
                }

                placementRecorder?.Invoke(
                    sphere,
                    placements.GetRange(placements.Count - sphere.items.Count, sphere.items.Count));
            }

            Debug.Assert(queue.Count == 0);
            return placements;
        }
    }

}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RandomizerCore.Collections;
using RandomizerCore.Exceptions;

namespace RandomizerCore.Randomizers
{
    [Obsolete]
    public abstract class TransitionPlacementStrategy
    {
        public abstract List<TransitionPlacement> Export(TransitionInitializer ti, IList<TransitionSphere> spheres);
    }

    [Obsolete]
    public class DefaultTransitionPlacementStrategy : TransitionPlacementStrategy
    {
        /// <summary>
        ///Inputs: Current randomized priority and transition. Output: new transition priority. Ignored if null.
        /// </summary>
        public Func<int, OldRandoTransition, int> depthPriorityTransform = null;
        /// <summary>
        /// If not null, invoked with the current depth and new placements.
        /// </summary>
        public Action<TransitionSphere, List<TransitionPlacement>> placementRecorder = null;

        public override List<TransitionPlacement> Export(TransitionInitializer ti, IList<TransitionSphere> spheres)
        {
            List<TransitionPlacement> placements = new();
            PriorityQueue<OldRandoTransition>[] queues = new PriorityQueue<OldRandoTransition>[ti.Length];
            for (int i = 0; i < queues.Length; i++) queues[i] = new();
            List<TransitionPlacement> current = new();

            for (int i = 0; i < spheres.Count; i++)
            {
                TransitionSphere sphere = spheres[i];
                foreach (OldRandoTransition transition in sphere.reachableTransitions)
                {
                    if (depthPriorityTransform != null)
                    {
                        transition.priority = depthPriorityTransform(i, transition);
                    }

                    queues[transition.dir].Enqueue(transition.priority, transition);
                }

                foreach (OldRandoTransition t2 in sphere.placedTransitions)
                {
                    if (!queues[t2.targetDir].TryExtractMin(out _, out OldRandoTransition t1))
                    {
                        throw new OutOfLocationsException("Ran out of transitions during Export (progression spheres).");
                    }

                    current.Add(new(t1, t2));

                    if (t1.coupled || t2.coupled)
                    {
                        current.Add(new(t2, t1));
                    }
                }

                placementRecorder?.Invoke(sphere, current);
                placements.AddRange(current);
                current.Clear();
            }

            // This cleans up unpaired reachable source transitions
            // There shouldn't be any decoupled transitions left over at this point
            for (int i = 0; i < queues.Length; i++)
            {
                while (queues[i].TryExtractMin(out _, out OldRandoTransition t1))
                {
                    if (!queues[t1.targetDir].TryExtractMin(out _, out OldRandoTransition t2))
                    {
                        throw new OutOfLocationsException("Ran out of transitions during Export (post-spheres).");
                    }

                    current.Add(new(t1, t2));
                    if (t1.coupled || t2.coupled) current.Add(new(t2, t1));
                }
            }

            if (placementRecorder != null)
            {
                TransitionSphere fakeSphere = new()
                {
                    index = spheres.Count,
                    directionCounts = spheres[^1].directionCounts,
                    placedTransitions = new(),
                    reachableTransitions = new(),
                };
                placementRecorder.Invoke(fakeSphere, current);
            }

            placements.AddRange(current);

            return placements;
        }
    }
}

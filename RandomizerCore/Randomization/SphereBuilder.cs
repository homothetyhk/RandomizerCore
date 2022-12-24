using RandomizerCore.Exceptions;
using RandomizerCore.Logic;
using System.Diagnostics;

namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Class which manages stepping the randomizer: selecting sets of items which unlock new locations until all locations are reachable. Spheres record the result of each step.
    /// </summary>
    public class SphereBuilder
    {
        public List<Sphere[]> Spheres = new();
        private readonly RandomizationStage stage;
        private readonly RandomizationGroup[] groups;
        private readonly CombinedItemSelector selector;
        private readonly CombinedLocationTracker rt;
        private readonly ProgressionManager pm;
        public List<RandoPlacement>[] Placements;
        private readonly TempState tempState;

        /// <summary>
        /// Creates a new SphereBuilder, using the ProgressionManager to monitor reachable locations.
        /// </summary>
        public SphereBuilder(RandomizationStage stage, ProgressionManager pm, TempState tempState)
        {
            this.stage = stage;
            this.groups = stage.groups;
            this.pm = pm;
            this.selector = new(groups);
            this.rt = new(pm, groups);
            this.Placements = new List<RandoPlacement>[groups.Length];
            for (int i = 0; i < groups.Length; i++) this.Placements[i] = new();
            this.tempState = tempState;
        }

        /// <summary>
        /// Steps the SphereBuilder to completion, putting the results into the Placements list.
        /// </summary>
        public void Advance()
        {
            pm.mu.StartUpdating();
            AddItemlessSphere();
            while (!selector.Finished)
            {
                AddNextSphere();
            }
        }

        /// <summary>
        /// Creates a sphere with the newest reachable locations and no items. Exits if the request was empty.
        /// </summary>
        /// <exception cref="InvalidOperationException">There are no reachable locations, and the request was nonempty.</exception>
        public void AddItemlessSphere()
        {
            if (!rt.FoundNew && Spheres.Count == 0)
            {
                if (groups.Any(g => g.Items.Any() || g.Locations.Any()))
                {
                    throw new OutOfLocationsException("Nonempty randomizer request has no reachable locations at start!");
                }
                else return;
            }

            rt.Collect(out List<IRandoLocation>[] newLocations);
            Sphere[] next = new Sphere[groups.Length];
            bool final = selector.Finished;

            for (int i = 0; i < next.Length; i++)
            {
                next[i] = new Sphere
                {
                    depth = Spheres.Count,
                    groupIndex = i,
                    final = final,
                    groupLabel = groups[i].Label,
                    Items = new(0),
                    Locations = newLocations[i],
                };
            }

            foreach (Sphere s in next)
            {
                foreach (IRandoLocation rl in s.Locations)
                {
                    rl.Sphere = s.depth;
                }
            }

            Spheres.Add(next);
            selector.UpdateCaps(next);
            PlaceSphere(next);
        }

        /// <summary>
        /// Steps the builder and creates a sphere with the forced items and unlocked locations.
        /// </summary>
        public void AddNextSphere()
        {
            Step(out List<IRandoItem>[] newItems);
            rt.Collect(out List<IRandoLocation>[] newLocations);
            Sphere[] next = new Sphere[groups.Length];
            bool final = selector.Finished;

            for (int i = 0; i < next.Length; i++)
            {
                next[i] = new Sphere
                {
                    depth = Spheres.Count,
                    groupIndex = i,
                    final = final,
                    groupLabel = groups[i].Label,
                    Items = newItems[i],
                    Locations = newLocations[i],
                };
            }

            bool finished = selector.Finished;
            foreach (Sphere s in next)
            {
                foreach (IRandoItem ri in s.Items)
                {
                    ri.Sphere = s.depth;
                    ri.Required = !finished;
                }
                foreach (IRandoLocation rl in s.Locations)
                {
                    rl.Sphere = s.depth;
                }
            }

            Spheres.Add(next);
            selector.UpdateCaps(next);
            PlaceSphere(next);
        }

        private void PlaceSphere(Sphere[] spheres)
        {
            List<RandoPlacement>[] placements = stage.strategy.PlaceItems(stage, spheres, tempState);
            if (!selector.Finished)
            {
                for (int i = 0; i < placements.Length; i++)
                {
                    for (int j = 0; j < placements[i].Count; j++)
                    {
                        (IRandoItem ri, IRandoLocation rl) = placements[i][j];
                        if (ri is ILocationDependentItem ildi)
                        {
                            pm.AddLocationDependentEffect(ildi, rl);
                        }
                    }
                }
            }
            for (int i = 0; i < placements.Length; i++) Placements[i].AddRange(placements[i]);
            if (rt.FoundNew) // possible due to ILocationDependentItems
            {
                AddItemlessSphere();
            }
        }

        [Conditional("DEBUG")]
        internal void LogSphere()
        {
            Log();
            Log("==================================");
            int i = Spheres.Count - 1;
            Log($"SPHERE {i}");
            Log("Placed:");
            for (int j = 0; j < groups.Length; j++)
            {
                if (Spheres[i][j].Items.Count > 0)
                {
                    Log($"  {groups[j].Label}: {string.Join(", ", Spheres[i][j].Items.Select(i => i.Name))}");
                }
            }
            Log("Unlocked:");
            for (int j = 0; j < groups.Length; j++)
            {
                if (Spheres[i][j].Locations.Count > 0)
                {
                    Log($"  {groups[j].Label}: {string.Join(", ", Spheres[i][j].Locations.Select(i => i.Name))}");
                }
            }
            Log("Current placements:");
            for (int j = 0; j < Placements.Length; j++)
            {
                Log(groups[j].Label);
                for (int k = 0; k < Placements[j].Count; k++)
                {
                    Log(Placements[j][k].Item.Name + "  at  " + Placements[j][k].Location.Name);
                }
            }

            Log("Progression:");
            Log(pm.Dump());
            Log("===============================");
        }


        [Conditional("DEBUG")]
        internal void LogSpheres()
        {
            Log();
            Log("BEGIN SPHERES");
            Log("==================================");
            for (int i = 0; i < Spheres.Count; i++)
            {
                Log($"SPHERE {i}");
                Log("Placed:");
                for (int j = 0; j < groups.Length; j++)
                {
                    if (Spheres[i][j].Items.Count > 0)
                    {
                        Log($"  {groups[j].Label}: {string.Join(", ", Spheres[i][j].Items.Select(i => i.Name))}");
                    }
                }
                Log("Unlocked:");
                for (int j = 0; j < groups.Length; j++)
                {
                    if (Spheres[i][j].Locations.Count > 0)
                    {
                        Log($"  {groups[j].Label}: {string.Join(", ", Spheres[i][j].Locations.Select(i => i.Name))}");
                    }
                }
            }
            Log("===============================");
            Log("END SPHERES");
            Log();
            Log("BALANCE");
            Log("===============================");
            int[] items = new int[groups.Length];
            int[] locations = new int[groups.Length];

            Log($"Total: {string.Join(", ", groups.Select(g => $"{g.Items.Length}/{g.Locations.Length}"))}");

            for (int i = 0; i < Spheres.Count; i++)
            {
                for (int j = 0; j < groups.Length; j++)
                {
                    items[j] += Spheres[i][j].Items.Count;
                    locations[j] += Spheres[i][j].Locations.Count;
                }

                Log($"[{i}]  items: {string.Join(", ", groups.Select((g, j) => g.Label.Substring(0, 4) + " " + items[j]))}");
                Log($"[{i}]  locations: {string.Join(", ", groups.Select((g, j) => g.Label.Substring(0, 4) + " " + locations[j]))}");
                Log();
            }
        }

        private static void Place(IRandoItem r, ProgressionManager pm)
        {
            r.Placed = TempState.Temporary;
            pm.Add(r);
        }

        private void Step(out List<IRandoItem>[] placed)
        {
            if (pm.Temp) throw new InvalidOperationException("Previous temp was not disposed!");
            pm.StartTemp();

            while (selector.TryProposeNext(out IRandoItem t))
            {
                Place(t, pm);
                if (rt.FoundNew)
                {
                    break;
                }
            }

            if (!rt.FoundNew)
            {
                // no locations were unlocked by adding items
                // we check that no locations are blocked by added items
                // specifically, audit checks for the case that a later proposal unlocks a location which was also a proposed item (e.g. a later proposed transition unlocks a coupled proposed transition)
                while (selector.TryRecallLast(out IRandoItem t, out bool coupled))
                {
                    if (coupled && Audit(t))
                    {
                        selector.RejectCurrentAndUnacceptAll();
                        break;
                    }
                    else
                    {
                        selector.AcceptLast();
                    }
                }

                if (!rt.FoundNew)
                {
                    // any locations which are not reachable at this point are unreachable
                    // randomization will fail during placement, so better to throw early and provide info
                    List<IRandoLocation>[] unreachable = rt.FindNonreachableLocations();
                    for (int i = 0; i < groups.Length; i++)
                    {
                        if (unreachable[i].Count != 0)
                        {
                            throw new UnreachableLocationException(unreachable, stage, tempState, pm);
                        }
                    }

                    // Nothing found by audit, so this is the last step, and we output all remaining items, unlocking no locations
                    try
                    {
                        selector.Finish(out placed);
                    }
                    catch (OutOfLocationsException oole)
                    {
                        throw new OutOfLocationsException("Ran out of locations on final step", oole);
                    }
                    
                    pm.SaveTempItems();
                    return;
                }
            }

            while (selector.TryRecallLast(out IRandoItem t))
            {
                if (Decide(t) && IsFinalItem())
                {
                    selector.RejectAllRemaining();
                    break;
                }
                if (!rt.FoundNew) throw new InvalidOperationException("Decide deleted necessary transition?!?!");
            }

            try
            {
                selector.FinishAccepting(out placed);
            }
            catch (OutOfLocationsException oole)
            {
                throw new OutOfLocationsException($"Used too many locations to unlock {string.Join(", ", rt.FindNonreachableLocations().SelectMany(l => l.Select(rl => rl.Name)))}" , oole);
            }

            pm.SaveTempItems();
            return;

            // used after new reachable locations have been found to determine which items are required for the access
            // returns false if there are still new reachable locations after restricting to the accepted items and the proposed items other than the current item
            // returns true otherwise
            bool Decide(IRandoItem t)
            {
                t.Placed = TempState.None;
                pm.RestrictTempTo(selector.GetTestItems());
                if (rt.FoundNew)
                {
                    selector.RejectLast(); // sets placed to none
                    return false;
                }
                else
                {
                    Place(t, pm); // sets placed to temporary
                    selector.AcceptLast(); // sets placed to permanent
                    if (!rt.FoundNew) throw new InvalidOperationException("Lost new transitions during decide?!?! Last accepted was " + selector.GetAcceptedItems().Last().Name);
                    return true;
                }
            }

            // used in a situation where all unused items together failed to make progress
            // tests a coupled item to determine whether its corresponding location becomes reachable with all other proposed items
            // in this case, we can reject the current item and do the normal decision process with the remaining items.
            bool Audit(IRandoItem t)
            {
                t.Placed = TempState.None;
                pm.RestrictTempTo(selector.GetTestItems());
                if (rt.FoundNew)
                {
                    // audit succeeded
                    return true;
                }
                else
                {
                    // audit failed
                    Place(t, pm); // resets state to as before audit
                    return false;
                }
            }

            // used after an item is determined to be required by Decide
            // returns true if all remaining proposed items can be rejected (the current accepted items suffice to unlock new reachable locations)
            bool IsFinalItem()
            {
                pm.RestrictTempTo(selector.GetAcceptedItems());
                if (rt.FoundNew)
                {
                    return true;
                }
                else
                {
                    pm.Add(selector.GetProposedItems());
                    if (!rt.FoundNew) throw new InvalidOperationException("Lost new transitions during IsFinalItem?!?!");
                    return false;
                }
            }
        }
    }
}

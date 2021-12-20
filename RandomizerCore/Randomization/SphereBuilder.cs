using RandomizerCore.Exceptions;
using RandomizerCore.Logic;

namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Class which manages stepping the randomizer: selecting sets of items which unlock new locations until all locations are reachable. Spheres record the result of each step.
    /// </summary>
    public class SphereBuilder
    {
        public List<Sphere[]> Spheres = new();
        private readonly RandomizationGroup[] groups;
        private readonly CombinedItemSelector selector;
        private readonly CombinedLocationTracker rt;
        private readonly ProgressionManager pm;
        private readonly MainUpdater mu;
        // TODO: doublecheck empty case handling

        /// <summary>
        /// Creates a new SphereBuilder, using the ProgressionManager and MainUpdater to monitor reachable locations.
        /// </summary>
        public SphereBuilder(RandomizationGroup[] groups, ProgressionManager pm, MainUpdater mu)
        {
            this.groups = groups;
            this.pm = pm;
            this.mu = mu;
            this.selector = new(groups);
            this.rt = new(mu, groups);
        }

        /// <summary>
        /// Steps the SphereBuilder to completion, putting the results into the Spheres list.
        /// </summary>
        public void Advance()
        {
            mu.Hook(pm);
            AddSphereZero();
            while (!selector.Finished)
            {
                AddNextSphere();
            }
            //LogSpheres();
        }

        /// <summary>
        /// Creates a sphere with the newest reachable locations and no items. Exits if the request was empty.
        /// </summary>
        /// <exception cref="InvalidOperationException">There are no reachable locations, and the request was nonempty.</exception>
        public void AddSphereZero()
        {
            if (!rt.FoundNew)
            {
                if (groups.Any(g => g.Items.Any() || g.Locations.Any()))
                {
                    throw new InvalidOperationException("Nonempty randomizer request has no reachable locations at start!");
                }
                else return;
            }

            rt.Collect(out List<IRandoLocation>[] newLocations);
            Sphere[] next = new Sphere[groups.Length];
            
            for (int i = 0; i < next.Length; i++)
            {
                next[i] = new Sphere
                {
                    depth = 0,
                    groupIndex = i,
                    groupLabel = groups[i].Label,
                    Items = new(0),
                    Locations = newLocations[i],
                };
            }

            foreach (Sphere s in next)
            {
                foreach (IRandoLocation rl in s.Locations)
                {
                    rl.Sphere = 0;
                }
            }

            Spheres.Add(next);
            selector.UpdateCaps(next);
        }

        /// <summary>
        /// Steps the builder and creates a sphere with the forced items and unlocked locations.
        /// </summary>
        public void AddNextSphere()
        {
            Step(out List<IRandoItem>[] newItems);
            rt.Collect(out List<IRandoLocation>[] newLocations);
            Sphere[] next = new Sphere[groups.Length];
            for (int i = 0; i < next.Length; i++)
            {
                next[i] = new Sphere
                {
                    depth = Spheres.Count,
                    groupIndex = i,
                    groupLabel = groups[i].Label,
                    Items = newItems[i],
                    Locations = newLocations[i],
                };
            }

            int sphere = groups.Length;
            bool finished = selector.Finished;
            foreach (Sphere s in next)
            {
                foreach (IRandoItem ri in s.Items)
                {
                    ri.Sphere = sphere;
                    ri.Required = !finished;
                }
                foreach (IRandoLocation rl in s.Locations)
                {
                    rl.Sphere = sphere;
                }
            }

            Spheres.Add(next);
            selector.UpdateCaps(next);
        }

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
            r.Placed = State.Temporary;
            pm.Add(r);
        }

        private void Step(out List<IRandoItem>[] placed)
        {
            if (pm.Temp) throw new InvalidOperationException("Previous temp was not disposed!");
            pm.StartTemp();

            while (selector.TryProposeNext(out IRandoItem t))
            {
                Place(t, pm);
                if (rt.FoundNew) break;
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
                    if (unreachable.Any(l => l.Count != 0))
                    {
                        throw new UnreachableLocationException(unreachable, groups);
                    }

                    // Nothing found by audit, so this is the last step, and we output all remaining items, unlocking no locations
                    selector.UnacceptAll();
                    selector.Finish(out placed);
                    pm.SaveTempItems();
                    return;
                }
            }

            while (selector.TryRecallLast(out IRandoItem t))
            {
                if (Decide(t) && IsFinalItem())
                {
                    break;
                }
                if (!rt.FoundNew) throw new Exception("Decide deleted necessary transition?!?!");
            }

            selector.FinishAccepting(out placed);
            pm.SaveTempItems();
            return;

            // used after new reachable locations have been found to determine which items are required for the access
            // returns false if there are still new reachable locations after restricting to the accepted items and the proposed items other than the current item
            // returns true otherwise
            bool Decide(IRandoItem t)
            {
                t.Placed = State.None;
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
                    if (!rt.FoundNew) throw new InvalidOperationException("Lost new transitions during decide?!?!");
                    return true;
                }
            }

            // used in a situation where all unused items together failed to make progress
            // tests a coupled item to determine whether its corresponding location becomes reachable with all other proposed items
            // in this case, we can reject the current item and do the normal decision process with the remaining items.
            bool Audit(IRandoItem t)
            {
                t.Placed = State.None;
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

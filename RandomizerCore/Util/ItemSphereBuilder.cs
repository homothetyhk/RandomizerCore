using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;
using static RandomizerCore.LogHelper;


namespace RandomizerCore
{
    [Obsolete]
    public class ItemSphere
    {
        public int index;

        /// <summary>
        /// The items to be placed at this sphere or earlier. These unlock the locations of the next sphere.
        /// </summary>
        public List<RandoItem> items;
        /// <summary>
        /// The locations newly unlocked at this sphere by the items of all previous spheres.
        /// </summary>
        public List<RandoLocation> locations;

        public string PrintSphereOpen()
        {
            StringBuilder sb = new();
            sb.AppendLine($"OPENED SPHERE {index}");
            sb.AppendLine($"REACHABLE: {string.Join(", ", locations.Select(t => t.Name))}");
            return sb.ToString();
        }

        public string PrintSphereClose()
        {
            StringBuilder sb = new();
            sb.AppendLine($"CLOSED SPHERE {index}");
            sb.AppendLine($"PLACED: {string.Join(", ", items.Select(t => t.Name))}");
            return sb.ToString();
        }

    }

    [Obsolete]
    public class ItemSphereBuilder
    {
        readonly List<ItemSphere> spheres = new();

        public List<ItemSphere> SphereList => spheres;

        public int SphereCount => spheres.Count;
        public ItemSphere LastSphere => spheres[^1];

        public ItemSphereBuilder()
        {
        }

        public void OpenSphere(List<RandoLocation> locations)
        {
            ItemSphere sphere = new();
            sphere.index = spheres.Count;
            sphere.locations = locations;
            spheres.Add(sphere);
            LogDebug(sphere.PrintSphereOpen());
        }

        public void CloseSphere(List<RandoItem> items, ProgressionManager pm)
        {
            ItemSphere sphere = spheres[^1];
            sphere.items = items;
            LogDebug(sphere.PrintSphereClose());
        }

        public ItemSphere GetSphere(int i) => spheres[i];

        public void BuildSpheres(ProgressionManager pm, ItemSelector selector, ReachableLocations rl)
        {
            while (!selector.Finished)
            {
                rl.Collect(out List<RandoLocation> newLocations);
                OpenSphere(newLocations);
                Step(pm, selector, rl, out List<RandoItem> newItems);
                CloseSphere(newItems, pm);
            }
        }

        public void ValidateSphereCounts()
        {
            int itemCount = spheres.Sum(s => s.items.Count);
            int locationCount = spheres.Sum(s => s.locations.Count);
            if (itemCount != locationCount)
            {
                throw new InvalidOperationException($"Item count {itemCount} does not agree with location count {locationCount} in ItemSphereBuilder with {SphereCount} spheres.");
            }
        }

        private static void Step(ProgressionManager pm, ItemSelector selector, ReachableLocations rl, out List<RandoItem> newItems)
        {
            bool Decide(RandoItem item)
            {
                LogDebug("Deciding for item " + item.Name);
                pm.RestrictTempTo(selector.GetProposedItems().Skip(1).Concat(selector.GetAcceptedItems()));
                if (rl.FoundNewLocations)
                {
                    selector.RejectLast();
                    return false;
                }
                else
                {
                    selector.AcceptLast();
                    pm.Add(item);
                    return true;
                }
            }

            bool IsFinalItem()
            {
                pm.RestrictTempTo(selector.GetAcceptedItems());
                LogDebug(string.Join(", ", selector.GetAcceptedItems()));
                if (rl.FoundNewLocations)
                {
                    LogDebug("Was final item--exiting loop.");
                    return true;
                }
                else
                {
                    LogDebug("Was not final item--continuing...");
                    pm.Add(selector.GetProposedItems());
                    return false;
                }
            }

            pm.StartTemp();
            pm.Add(selector.Resume());
            if (!rl.FoundNewLocations)
            {
                while (selector.TryProposeNext(out RandoItem item))
                {
                    pm.Add(item);
                    if (rl.FoundNewLocations) break;
                }

                if (!rl.FoundNewLocations)
                {
                    selector.Finish(out newItems);
                    pm.SaveTempItems();
                    return;
                }

                selector.AcceptLast();

                if (IsFinalItem())
                {
                    selector.FinishAccepting(out newItems);
                    pm.SaveTempItems();
                    return;
                }
            }

            while (selector.TryRecallLast(out RandoItem item))
            {
                if (Decide(item) && IsFinalItem())
                {
                    break;
                }
            }

            selector.FinishAccepting(out newItems);
            pm.SaveTempItems();
        }
    }
}

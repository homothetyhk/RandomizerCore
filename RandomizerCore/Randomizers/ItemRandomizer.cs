using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Extensions;
using RandomizerCore.Logic;
using RandomizerCore.Collections;
using static RandomizerCore.LogHelper;
using RandomizerCore.Exceptions;
using System.Diagnostics;

namespace RandomizerCore.Randomizers
{
    public class ItemRandomizer
    {
        public readonly Random rng;
        readonly LogicManager lm;
        readonly IItemRandomizerSettings rs;
        readonly RandoContext ctx;
        readonly RandoMonitor rm;

        List<RandoItem> items;
        List<RandoLocation> locations;

        ProgressionManager pm;

        MainUpdater updater;

        ReachableLocations rl;
        ItemSelector itemSelector;
        ItemSphereBuilder isb;
        readonly ItemPlacementStrategy ips;

        List<ItemPlacement> itemPlacements;

        public ItemRandomizer(IItemRandomizerSettings rs, RandoContext ctx, RandoMonitor rm = null)
        {
            this.rs = rs;
            this.ctx = ctx;
            this.rm = rm ??= new();
            this.lm = rs.LM;
            rng = new Random(rs.Seed);

            rm.SendEvent(RandoEventType.Initializing);
            rs.Initialize(rng);
            ips = rs.GetItemPlacementStrategy() ?? new DefaultItemPlacementStrategy();
            items = rs.GetRandomizedItems();
            locations = rs.GetRandomizedLocations();

            Permute();

            pm = new ProgressionManager(lm, rs, ctx);
            updater = new MainUpdater(lm);
            updater.AddPlacements(lm.Waypoints);
            updater.AddPlacements(rs.GetVanillaPlacements());
            if (ctx.transitionPlacements != null) updater.AddPlacements(ctx.transitionPlacements);
            if (ctx.itemPlacements != null) updater.AddPlacements(ctx.itemPlacements);
            else ctx.itemPlacements = new(items.Count);
            rl = new ReachableLocations(updater, locations);
        }

        public void Run()
        {
            while (true)
            {
                try
                {
                    rm.SendEvent(RandoEventType.NewAttempt);
                    ItemPass();
                    break;
                }
                catch (OutOfLocationsException e)
                {
                    rm.SendEvent(RandoEventType.Error, e.ToString());
                    Reset();
                }
            }

            rm.SendEvent(RandoEventType.Validating);
            Validate();
            ctx.itemPlacements.AddRange(itemPlacements);
            rm.SendEvent(RandoEventType.Finished);
        }

        private void ItemPass()
        {
            updater.Hook(pm);
            itemSelector = new ItemSelector(items);
            isb = new ItemSphereBuilder();
            isb.BuildSpheres(pm, itemSelector, rl);
            itemPlacements = ips.Export(isb.SphereList);
        }

        private void Permute()
        {
            rng.PermuteInPlace(items, (i, p) => i.priority = p);
            rng.PermuteInPlace(locations, (l, p) => l.priority = p);

            rs.PostPermuteItems(rng, items);
            rs.PostPermuteLocations(rng, locations);

            items.StableSort((i, j) => j.priority - i.priority); // items are sorted in reverse since they are immediately loaded into a stack
            locations.StableSort((i, j) => i.priority - j.priority);
        }

        public void Reset()
        {
            pm.Reset();
            updater.Reset();
            Permute();
        }

        private void Validate()
        {
            Log("Beginning validation.");

            if (!itemPlacements.Select(i => i.item.Name).OrderBy(s => s).SequenceEqual(items.Select(i => i.Name).OrderBy(s => s)))
            {
                throw new ValidationException("Placed items do not match randomized items.");
            }

            pm.Reset();
            MainUpdater updater = new MainUpdater(lm);
            updater.AddPlacements(rs.GetVanillaPlacements());
            updater.AddPlacements(lm.Waypoints);
            updater.AddPlacements(ctx.itemPlacements);
            if (ctx.transitionPlacements != null) updater.AddPlacements(ctx.transitionPlacements);

            List<PrePlacedItemUpdateEntry> itemEntries = itemPlacements.Select(p => new PrePlacedItemUpdateEntry(p)).ToList();
            foreach (var entry in itemEntries) updater.AddEntry(entry);

            updater.Hook(pm);
            LogDebug(pm.ToString());

            foreach (var entry in itemEntries)
            {
                if (!entry.obtained) throw new ValidationException("Unreachable item placement detected.");
            }
            Log("Validation completed successfully!");
        }


    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;
using RandomizerCore;
using RandomizerCore.Extensions;
using RandomizerCore.Logic;
using static RandomizerCore.LogHelper;
using RandomizerCore.Collections;
using RandomizerCore.Exceptions;

namespace RandomizerCore.Randomizers
{
    public class TransitionRandomizer
    {
        public readonly Random rng;
        readonly LogicManager lm;
        readonly ITransitionRandomizerSettings rs;
        readonly RandoContext ctx;
        readonly RandoMonitor rm;

        readonly List<RandoItem> items;
        List<RandoLocation> locations;
        readonly List<RandoTransition> transitions;

        ProgressionManager pm;

        MainUpdater firstUpdater;
        ReachableLocations rl;
        ItemSelector itemSelector;
        ItemSphereBuilder isb;
        ItemPlacementStrategy ips;

        MainUpdater secondUpdater;
        TransitionInitializer ti;
        ReachableTransitions rt;
        TransitionSelector transitionSelector;
        TransitionSphereBuilder tsb;
        TransitionPlacementStrategy tps;

        List<TransitionPlacement> transitionPlacements;
        List<ItemPlacement> tempItemPlacements;
        List<ItemPlacement> finalItemPlacements;

        public TransitionRandomizer(ITransitionRandomizerSettings rs, RandoContext ctx, RandoMonitor rm = null)
        {
            this.rs = rs;
            this.lm = rs.LM;
            this.ctx = ctx;
            this.rm = rm ??= new();
            rng = new Random(rs.Seed);

            rm.SendEvent(RandoEventType.Initializing);

            rs.Initialize(rng);
            ips = rs.GetItemPlacementStrategy() ?? new DefaultItemPlacementStrategy();
            tps = rs.GetTransitionPlacementStrategy() ?? new DefaultTransitionPlacementStrategy();

            items = rs.GetRandomizedItems();
            locations = rs.GetRandomizedLocations();
            transitions = rs.GetRandomizedTransitions();

            Permute();

            firstUpdater = new(lm);
            secondUpdater = new(lm);
        }

        public void Run()
        {
            while (true)
            {
                try
                {
                    rm.SendEvent(RandoEventType.NewAttempt);
                    ItemPassOne();
                    TransitionPassOne();
                    ItemPassTwo();
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
            ctx.transitionPlacements.AddRange(transitionPlacements);
            ctx.itemPlacements.AddRange(finalItemPlacements);
            rm.SendEvent(RandoEventType.Finished);
        }

        private void Reset()
        {
            pm.Reset();
            firstUpdater.Reset();
            secondUpdater.Clear();
            ti.Reset(transitions);
            Permute();
        }

        private void Permute()
        {
            rng.PermuteInPlace(items, (i, p) => i.priority = p);
            rng.PermuteInPlace(locations, (l, p) => l.priority = p);
            rng.PermuteInPlace(transitions, (t, p) => t.priority = p);

            rs.PostPermuteItems(rng, items);
            rs.PostPermuteLocations(rng, locations);

            // Note - ti must be initialized once transition order is final
            ti = new TransitionInitializer(rs, lm);
            ti.Initialize(transitions);
            // initialize ti first so transitions have accurate directions
            rs.PostPermuteTransitions(rng, transitions);

            items.StableSort((i, j) => j.priority - i.priority); // items are sorted in reverse since they are immediately loaded into a stack
            locations.StableSort((i, j) => i.priority - j.priority);
            transitions.StableSort((i, j) => j.priority - i.priority); // transitions are sorted in reverse since they are immediately loaded into a stack
        }

        private void ItemPassOne()
        {
            pm = new ProgressionManager(lm, rs, ctx);
            firstUpdater = new MainUpdater(lm);
            firstUpdater.AddPlacements(lm.Waypoints);
            firstUpdater.AddPlacements(rs.GetVanillaPlacements());
            if (ctx.transitionPlacements != null) firstUpdater.AddPlacements(ctx.transitionPlacements);
            else ctx.transitionPlacements = new(transitions.Count);
            if (ctx.itemPlacements != null) firstUpdater.AddPlacements(ctx.itemPlacements);
            else ctx.itemPlacements = new(items.Count);

            foreach (var t in transitions) pm.Add(t);

            rl = new ReachableLocations(firstUpdater, locations);

            firstUpdater.Hook(pm);
            itemSelector = new ItemSelector(items);
            isb = new ItemSphereBuilder();
            isb.BuildSpheres(pm, itemSelector, rl);
            isb.ValidateSphereCounts();

            tempItemPlacements = ips.Export(isb.SphereList);
        }

        private void TransitionPassOne()
        {
            pm.Reset();
            // TODO: recycle first updater
            secondUpdater = new MainUpdater(lm);
            secondUpdater.AddPlacements(lm.Waypoints);
            secondUpdater.AddPlacements(rs.GetVanillaPlacements());
            secondUpdater.AddPlacements(tempItemPlacements);
            if (ctx.transitionPlacements != null) firstUpdater.AddPlacements(ctx.transitionPlacements);
            if (ctx.itemPlacements != null) firstUpdater.AddPlacements(ctx.itemPlacements);

            rt = new ReachableTransitions(secondUpdater, transitions);

            secondUpdater.Hook(pm);
            transitionSelector = new TransitionSelector(transitions);
            tsb = new TransitionSphereBuilder(ti);

            tsb.BuildSpheres(pm, transitionSelector, rt);
            tsb.ValidateSphereCount();

            PriorityQueue<RandoTransition>[] directedReachable = new PriorityQueue<RandoTransition>[ti.Length];
            for (int i = 0; i < ti.Length; i++) directedReachable[i] = new PriorityQueue<RandoTransition>();

            transitionPlacements = tps.Export(ti, tsb.SphereList);
        }

        private void ItemPassTwo()
        {
            pm.Reset();
            MainUpdater updater = new(lm);
            updater.AddPlacements(lm.Waypoints);
            updater.AddPlacements(rs.GetVanillaPlacements());
            updater.AddPlacements(ctx.transitionPlacements);
            updater.AddPlacements(transitionPlacements);
            if (ctx.itemPlacements != null) updater.AddPlacements(ctx.itemPlacements);
            rl = new(updater, locations);
            updater.Hook(pm);

            itemSelector = new(items);
            isb = new();
            isb.BuildSpheres(pm, itemSelector, rl);
            isb.ValidateSphereCounts();

            finalItemPlacements = ips.Export(isb.SphereList);
        }

        /// <summary>
        /// Tests the randomizer output and sends an exception if it is invalid.
        /// </summary>
        /// <exception cref="ValidationException"></exception>
        public void Validate()
        {
            Log("Beginning validation.");
            
            if (!finalItemPlacements.Select(i => i.item.Name).OrderBy(s => s).SequenceEqual(items.Select(i => i.Name).OrderBy(s => s)))
            {
                throw new ValidationException("Placed items do not match randomized items.");
            }

            if (!finalItemPlacements.Select(i => i.location.Name).OrderBy(s => s).SequenceEqual(locations.Select(i => i.Name).OrderBy(s => s)))
            {
                throw new ValidationException("Placed locations do not match randomized locations.");
            }

            if (!transitionPlacements.Select(p => p.source.Name).OrderBy(s => s).SequenceEqual(transitions.Where(t => t.dir != ti.oneWayOut).Select(t => t.Name).OrderBy(s => s)))
            {
                throw new ValidationException("Placed source transitions do not match randomized source transitions.");
            }

            if (!transitionPlacements.Select(p => p.target.Name).OrderBy(s => s).SequenceEqual(transitions.Where(t => t.dir != ti.oneWayIn).Select(t => t.Name).OrderBy(s => s)))
            {
                throw new ValidationException("Placed target transitions do not match randomized target transitions.");
            }

            pm.Reset();
            MainUpdater updater = new MainUpdater(lm);
            updater.AddPlacements(rs.GetVanillaPlacements());
            updater.AddPlacements(lm.Waypoints);
            if (ctx.itemPlacements != null) updater.AddPlacements(ctx.itemPlacements);
            if (ctx.transitionPlacements != null) updater.AddPlacements(ctx.transitionPlacements);
            List<PrePlacedItemUpdateEntry> itemEntries = finalItemPlacements.Select(p => new PrePlacedItemUpdateEntry(p)).ToList();
            List<TransitionUpdateEntry> transitionEntries = transitionPlacements.Select(p => new TransitionUpdateEntry(p)).ToList();
            foreach (var entry in itemEntries) updater.AddEntry(entry);
            foreach (var entry in transitionEntries) updater.AddEntry(entry);

            var transitionLookup = new Dictionary<string, TransitionUpdateEntry>();
            for (int i = 0; i < transitionPlacements.Count; i++)
            {
                transitionLookup[transitionPlacements[i].source.Name] = transitionEntries[i];
                transitionLookup[transitionPlacements[i].target.Name] = transitionEntries[i];
            }

            updater.Hook(pm);
            LogDebug("Progression after validation update:");
            LogDebug(pm.ToString());

            foreach (RandoTransition t in transitions)
            {
                if (!pm.Has(t.lt.term.Id))
                {
                    string t1 = transitionPlacements.FirstOrDefault(p => p.target == t).source?.Name;
                    throw new ValidationException($"Transition {t.Name} (source: {t1}) was not reachable.");
                }
            }
            foreach (var entry in itemEntries)
            {
                if (!entry.obtained) throw new ValidationException("Unreachable item placement detected.");
            }
            /*
            // Due to current limitations in transition logic, not all transition update entries are triggered
            foreach (var entry in transitionEntries)
            {
                if (!entry.obtained) throw new ValidationException("Unreachable transition placement detected.");
            }
            */
            Log("Validation completed successfully!");
        }
    }
}

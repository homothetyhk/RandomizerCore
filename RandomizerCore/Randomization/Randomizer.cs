using RandomizerCore.Exceptions;
using RandomizerCore.Extensions;
using RandomizerCore.Logic;

namespace RandomizerCore.Randomization
{
    public class Randomizer
    {
        public readonly LogicManager lm;
        public readonly RandomizationStage[] stages;
        public readonly RandoContext ctx;
        readonly ProgressionManager pm;
        public readonly Random rng;
        public readonly RandoMonitor rm;
        public readonly List<List<RandoPlacement>[]> stagedPlacements;

        public Randomizer(Random rng, RandoContext ctx, RandomizationStage[] stages, RandoMonitor rm = null)
        {
            this.ctx = ctx;
            this.lm = ctx.LM;
            this.rm = rm;
            this.stages = stages;
            this.rng = rng;
            stagedPlacements = new();
            pm = new(lm, ctx);
            foreach (RandomizationStage stage in stages)
            {
                foreach (RandomizationGroup group in stage.groups)
                {
                    if (group.Items.Length != group.Locations.Length) throw new ArgumentException($"Group {group.Label} in stage {stage.label} has nonmatching arrays!");
                }
            }
        }

        public List<List<RandoPlacement>[]> Run()
        {
            while (true)
            {
                try
                {
                    rm.SendEvent(RandoEventType.NewAttempt);
                    PermuteAll();
                    for (int i = 0; i < stages.Length - 1; i++) RandomizeForward(i, State.Temporary);
                    Randomize(stages.Length - 1, State.Permanent);
                    for (int i = stages.Length - 2; i >= 0; i--) Rerandomize(i, State.Permanent);
                    break;
                }
                catch (OutOfLocationsException e)
                {
                    rm.SendError(e);
                    Reset();
                }
                catch (UnreachableLocationException e)
                {
                    rm.SendError(e);
                    LogDebug(e.GetVerboseMessage());
                    Reset();
                }
            }

            rm.SendEvent(RandoEventType.Validating);
            Validate();
            rm.SendEvent(RandoEventType.Finished);
            return stagedPlacements;
        }

        private void PermuteAll()
        {
            for (int i = 0; i < stages.Length; i++)
            {
                Permute(stages[i]);
            }
        }

        private void Permute(RandomizationStage stage)
        {
            RandomizationGroup[] groups = stage.groups;

            for (int i = 0; i < groups.Length; i++)
            {
                float scale = groups[i].Items.Length;
                rng.PermuteInPlace(groups[i].Items, (i, p) => i.Priority = p / scale);
                rng.PermuteInPlace(groups[i].Locations, (i, p) => i.Priority = p / scale);
                groups[i].InvokeOnPermute(rng);

                groups[i].Items.StableSort(ComparerUtil.ItemComparer);
                Array.Reverse(groups[i].Items); // items are sorted in reverse since they will be loaded into a stack
                groups[i].Locations.StableSort(ComparerUtil.LocationComparer);
            }
        }

        private MainUpdater InitializeUpdater()
        {
            MainUpdater mu = new(lm);
            mu.AddPlacements(lm.Waypoints);
            mu.AddPlacements(ctx.EnumerateExistingPlacements());
            return mu;
        }

        private void ResetStage(RandomizationStage stage)
        {
            foreach (RandomizationGroup group in stage.groups) ResetGroup(group);
        }

        private void ResetGroup(RandomizationGroup group)
        {
            foreach (IRandoItem item in group.Items)
            {
                item.Placed = State.None;
                item.Sphere = -1;
                item.Required = false;
            }

            foreach (IRandoLocation location in group.Locations)
            {
                location.Reachable = State.None;
                location.Sphere = -1;
            }
        }

        // Randomizes the groups in the stage with the given index.
        // State can be Temporary or Permanent, or None if indeterminate.
        // All staged placements will be accounted for during randomization.
        // For stages of greater index, their items will be added directly to the ProgressionManager before randomization.
        // Finishes by adding a new entry to the stagedPlacements list.
        private void RandomizeForward(int index, State state)
        {
            RandomizationGroup[] groups = stages[index].groups;
            MainUpdater mu = InitializeUpdater();
            for (int i = 0; i < stagedPlacements.Count; i++)
            {
                foreach (List<RandoPlacement> m in stagedPlacements[i]) foreach (RandoPlacement n in m) mu.AddEntry(new PrePlacedItemUpdateEntry(n.Item, n.Location));
            }

            for (int i = index + 1; i < stages.Length; i++)
            {
                foreach (RandomizationGroup g in stages[i].groups) pm.Add(g.Items);
            }

            SphereBuilder sb = new(groups, pm, mu);
            sb.Advance();
            stagedPlacements.Add(stages[index].strategy.PlaceItems(stages[index], sb.Spheres, state));
            pm.Reset();
        }

        // Randomizes the groups in the stage with the given index.
        // State can be Temporary or Permanent, or None if indeterminate.
        // All staged placements will be accounted for during randomization.
        // No handling for stages of greater index.
        // Finishes by adding a new entry to the stagedPlacements list.
        private void Randomize(int index, State state)
        {
            RandomizationGroup[] groups = stages[index].groups;
            MainUpdater mu = InitializeUpdater();
            for (int i = 0; i < stagedPlacements.Count; i++)
            {
                foreach (List<RandoPlacement> m in stagedPlacements[i]) foreach (RandoPlacement n in m) mu.AddEntry(new PrePlacedItemUpdateEntry(n.Item, n.Location));
            }

            SphereBuilder sb = new(groups, pm, mu);
            sb.Advance();
            stagedPlacements.Add(stages[index].strategy.PlaceItems(stages[index], sb.Spheres, state));
            pm.Reset();
        }

        // Randomizes the groups in the stage with the given index.
        // State can be Temporary or Permanent, or None if indeterminate
        // Staged placements will be accounted for during randomization, except for the entry with the same index as the current stage which is ignored.
        // No handling for stages of greater index.
        // Finishes by overwriting the entry with the given index in stagedPlacements.
        private void Rerandomize(int index, State state)
        {
            ResetStage(stages[index]);
            RandomizationGroup[] groups = stages[index].groups;
            MainUpdater mu = InitializeUpdater();
            for (int i = 0; i < stagedPlacements.Count; i++)
            {
                if (i == index) continue;
                foreach (List<RandoPlacement> m in stagedPlacements[i]) foreach (RandoPlacement n in m) mu.AddEntry(new PrePlacedItemUpdateEntry(n.Item, n.Location));
            }

            SphereBuilder sb = new(groups, pm, mu);
            sb.Advance();
            stagedPlacements[index] = stages[index].strategy.PlaceItems(stages[index], sb.Spheres, state);
            pm.Reset();
        }

        public void Reset()
        {
            pm.Reset();
            stagedPlacements.Clear();
            foreach (RandomizationStage stage in stages) ResetStage(stage);
        }

        /// <summary>
        /// Tests the randomizer output and sends an exception if it is invalid.
        /// </summary>
        /// <exception cref="ValidationException"></exception>
        public void Validate()
        {
            pm.Reset();
            MainUpdater mu = InitializeUpdater();

            List<List<PrePlacedItemUpdateEntry>[]> entries = new();
            for (int i = 0; i < stages.Length; i++)
            {
                entries.Add(new List<PrePlacedItemUpdateEntry>[stages[i].groups.Length]);
                for (int j = 0; j < stages[i].groups.Length; j++)
                {
                    List<PrePlacedItemUpdateEntry> groupEntries = entries[i][j] = new();
                    foreach (RandoPlacement p in stagedPlacements[i][j])
                    {
                        PrePlacedItemUpdateEntry e = new(p.Item, p.Location);
                        groupEntries.Add(e);
                        mu.AddEntry(e);
                    }
                }
            }

            mu.Hook(pm);
            
            for (int i = 0; i < stages.Length; i++)
            {
                for (int j = 0; j < stages[i].groups.Length; j++)
                {
                    stages[i].groups[j].Validator.Validate(stages[i].groups[j], pm, stagedPlacements[i][j], entries[i][j]);
                }
            }
        }
    }
}

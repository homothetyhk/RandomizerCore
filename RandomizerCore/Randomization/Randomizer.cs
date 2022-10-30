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
            this.rm = rm ?? new();
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
            rm.SendEvent(RandoEventType.Initializing);
        }

        public List<List<RandoPlacement>[]> Run()
        {
            while (true)
            {
                try
                {
                    rm.SendEvent(RandoEventType.NewAttempt);
                    InitializeStages();
                    PermuteAll();
                    InitializeUpdater(pm.mu);
                    for (int i = 0; i < stages.Length - 1; i++) RandomizeForward(i, TempState.Temporary);
                    Randomize(stages.Length - 1, TempState.Permanent);
                    for (int i = stages.Length - 2; i >= 0; i--) Rerandomize(i, TempState.Permanent);
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

        private void InitializeStages()
        {
            for (int i = 0; i < stages.Length; i++)
            {
                stages[i].OnNewAttempt();
            }
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

        private void InitializeUpdater(MainUpdater mu)
        {
            mu.Clear();
            mu.AddWaypoints(lm.Waypoints);
            mu.AddTransitions(lm.TransitionLookup.Values);
            mu.AddPlacements(ctx.EnumerateExistingPlacements());
            mu.SetLongTermRevertPoint();
        }

        private void ResetStage(RandomizationStage stage)
        {
            foreach (RandomizationGroup group in stage.groups) ResetGroup(group);
            stage.strategy.Reset();
        }

        private void ResetGroup(RandomizationGroup group)
        {
            foreach (IRandoItem item in group.Items)
            {
                item.Placed = TempState.None;
                item.Sphere = -1;
                item.Required = false;
            }

            foreach (IRandoLocation location in group.Locations)
            {
                location.Reachable = TempState.None;
                location.Sphere = -1;
            }

            group.Strategy.Reset();
        }

        // Randomizes the groups in the stage with the given index.
        // State can be Temporary or Permanent, or None if indeterminate.
        // All staged placements will be accounted for during randomization.
        // For stages of greater index, their items will be added directly to the ProgressionManager before randomization.
        // Finishes by adding a new entry to the stagedPlacements list.
        private void RandomizeForward(int index, TempState state)
        {
            for (int i = 0; i < stagedPlacements.Count; i++)
            {
                foreach (List<RandoPlacement> m in stagedPlacements[i]) foreach (RandoPlacement n in m) pm.mu.AddEntry(new PrePlacedItemUpdateEntry(n.Item, n.Location));
            }

            for (int i = index + 1; i < stages.Length; i++)
            {
                foreach (RandomizationGroup g in stages[i].groups) pm.Add(g.Items);
            }

            SphereBuilder sb = new(stages[index], pm, state);
            sb.Advance();
            stagedPlacements.Add(sb.Placements);
            pm.Reset();
        }

        // Randomizes the groups in the stage with the given index.
        // State can be Temporary or Permanent, or None if indeterminate.
        // All staged placements will be accounted for during randomization.
        // No handling for stages of greater index.
        // Finishes by adding a new entry to the stagedPlacements list.
        private void Randomize(int index, TempState state)
        {
            for (int i = 0; i < stagedPlacements.Count; i++)
            {
                foreach (List<RandoPlacement> m in stagedPlacements[i]) foreach (RandoPlacement n in m) pm.mu.AddEntry(new PrePlacedItemUpdateEntry(n.Item, n.Location));
            }

            SphereBuilder sb = new(stages[index], pm, state);
            sb.Advance();
            stagedPlacements.Add(sb.Placements);
            pm.Reset();
        }

        // Randomizes the groups in the stage with the given index.
        // State can be Temporary or Permanent, or None if indeterminate
        // Staged placements will be accounted for during randomization, except for the entry with the same index as the current stage which is ignored.
        // No handling for stages of greater index.
        // Finishes by overwriting the entry with the given index in stagedPlacements.
        private void Rerandomize(int index, TempState state)
        {
            ResetStage(stages[index]);
            for (int i = 0; i < stagedPlacements.Count; i++)
            {
                if (i == index) continue;
                foreach (List<RandoPlacement> m in stagedPlacements[i]) foreach (RandoPlacement n in m) pm.mu.AddEntry(new PrePlacedItemUpdateEntry(n.Item, n.Location));
            }

            SphereBuilder sb = new(stages[index], pm, state);
            sb.Advance();
            stagedPlacements[index] = sb.Placements;
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
                        pm.mu.AddEntry(e);
                    }
                }
            }

            pm.mu.StartUpdating();
            
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

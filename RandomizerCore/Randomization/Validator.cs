using RandomizerCore.Exceptions;
using RandomizerCore.Logic;
using System.Text;

namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Base class which handles determining whether errors exist in the randomizer output for a group.
    /// <br/>By default, checks that the placement list has the right counts by name, and that all locations are reachable.
    /// </summary>
    public class Validator
    {
        /// <summary>
        /// Tests the randomizer output for the given group and sends an exception if it is invalid.
        /// </summary>
        /// <exception cref="ValidationException"></exception>
        public virtual void Validate(RandomizationGroup group, ProgressionManager pm, List<RandoPlacement> placements, List<PrePlacedItemUpdateEntry> entries)
        {
            ValidateCounts(group, pm, placements, entries);
            ValidateNameCounts(group, pm, placements, entries);
            ValidateAllLocationsReachable(group, pm, placements, entries);
        }

        protected virtual void ValidateCounts(RandomizationGroup group, ProgressionManager pm, List<RandoPlacement> placements, List<PrePlacedItemUpdateEntry> entries)
        {
            if (group.Items.Length > placements.Count)
            {
                throw new ValidationException($"Items deleted from final placement for randomization group {group.Label}. Group expected {group.Items.Length} placements, but has {placements.Count} placements.");
            }
            else if (group.Items.Length < placements.Count)
            {
                throw new ValidationException($"Too many items found in final placement for randomization group {group.Label}. Group expected {group.Items.Length} placements, but has {placements.Count} placements.");
            }
        }

        protected virtual void ValidateNameCounts(RandomizationGroup group, ProgressionManager pm, List<RandoPlacement> placements, List<PrePlacedItemUpdateEntry> entries)
        {
            Dictionary<string, int> nameCounts = new();
            foreach (IRandoItem r in group.Items)
            {
                nameCounts.TryGetValue(r.Name, out int value);
                nameCounts[r.Name] = value + 1;
            }
            foreach (RandoPlacement p in placements)
            {
                nameCounts.TryGetValue(p.Item.Name, out int value);
                nameCounts[p.Item.Name] = value - 1;
            }
            if (nameCounts.Any(kvp => kvp.Value != 0))
            {
                throw new ValidationException($"Improper item counts found in plcaements for randomization group {group.Label}: " +
                    $"nonzero net counts (requested minus placed) are {string.Join(", ", nameCounts.Where(kvp => kvp.Value != 0).Select(kvp => (kvp.Key, kvp.Value)))}");
            }
            nameCounts.Clear();
            foreach (IRandoLocation r in group.Locations)
            {
                nameCounts.TryGetValue(r.Name, out int value);
                nameCounts[r.Name] = value + 1;
            }
            foreach (RandoPlacement p in placements)
            {
                nameCounts.TryGetValue(p.Location.Name, out int value);
                nameCounts[p.Location.Name] = value - 1;
            }
            if (nameCounts.Any(kvp => kvp.Value != 0))
            {
                throw new ValidationException($"Improper location counts found in plcaements for randomization group {group.Label}: " +
                    $"nonzero net counts (requested minus placed) are {string.Join(", ", nameCounts.Where(kvp => kvp.Value != 0).Select(kvp => (kvp.Key, kvp.Value)))}");
            }
        }

        protected virtual void ValidateAllLocationsReachable(RandomizationGroup group, ProgressionManager pm, List<RandoPlacement> placements, List<PrePlacedItemUpdateEntry> entries)
        {
            StringBuilder sb = null;
            foreach (PrePlacedItemUpdateEntry entry in entries)
            {
                if (!entry.obtained)
                {
                    sb ??= new StringBuilder($"Unreachable placement(s) detected in group {group.Label}:").AppendLine();
                    sb.AppendLine($"  {entry.item.Name} at {entry.location.Name}");
                }
            }
            if (sb != null)
            {
                throw new ValidationException(sb.ToString());
            }
        }
    }
}

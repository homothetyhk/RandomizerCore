using RandomizerCore.Logic;
using System.Text;

namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Validator which assumes that its group is composed of RandoTransitions, and only checks that their terms are obtained, rather than that they are reachable.
    /// </summary>
    public class WeakTransitionValidator : Validator
    {
        public override void Validate(RandomizationGroup group, ProgressionManager pm, List<RandoPlacement> placements, List<PrePlacedItemUpdateEntry> entries)
        {
            ValidateCounts(group, pm, placements, entries);
            ValidateNameCounts(group, pm, placements, entries);
            WeakValidateTransitionsObtained(group, pm, placements, entries);
        }

        protected virtual void WeakValidateTransitionsObtained(RandomizationGroup group, ProgressionManager pm, List<RandoPlacement> placements, List<PrePlacedItemUpdateEntry> entries)
        {
            StringBuilder sb = null;
            foreach (RandoPlacement p in placements)
            {
                RandoTransition source = (RandoTransition)p.Location;
                RandoTransition target = (RandoTransition)p.Item;

                if (!pm.Has(source.lt.term))
                {
                    sb ??= new StringBuilder($"Inaccessible transitions(s) detected in group {group.Label}:").AppendLine();
                    sb.AppendLine($"  {p.Location.Name} in {p.Location.Name} --> {p.Item.Name}");
                }
                if (!pm.Has(target.lt.term))
                {
                    sb ??= new StringBuilder($"Inaccessible transitions(s) detected in group {group.Label}:").AppendLine();
                    sb.AppendLine($"  {p.Item.Name} in {p.Location.Name} --> {p.Item.Name}");
                }
            }
        }
    }
}

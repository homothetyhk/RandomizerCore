using RandomizerCore.Logic;

namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Location which indicates that the items of the group are assumed to be available for the purposes of randomization.
    /// <br/>Used with ILocationDependentItem effects under such circumstances.
    /// </summary>
    public sealed class IndeterminateLocation : RandoLocation
    {
        public RandomizationGroup Group { get; }
        public Dictionary<string, object> Shared { get; } = new();

        public IndeterminateLocation(LogicManager lm, RandomizationGroup group)
        {
            base.logic = lm.FromString(new("Indeterminate-" + group.Label, "TRUE"));
            this.Group = group;
        }
    }
}

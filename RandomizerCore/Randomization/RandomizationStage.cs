namespace RandomizerCore.Randomization
{
    /// <summary>
    /// A collection of RandomizationGroups which should be randomized concurrently by the randomizer, and placed according to the stage's strategy.
    /// </summary>
    public class RandomizationStage
    {
        public RandomizationGroup[] groups;
        public StagePlacementStrategy strategy;
        public string label;

        public override string ToString()
        {
            return label ?? base.ToString();
        }
    }
}

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

        /// <summary>
        /// Called on each new attempt of the randomizer. Base calls OnNewAttempt on each group in the stage.
        /// </summary>
        public virtual void OnNewAttempt() 
        {
            for (int i = 0; i < groups.Length; i++) groups[i].OnNewAttempt();
        }

        public override string ToString()
        {
            return label ?? base.ToString();
        }
    }
}

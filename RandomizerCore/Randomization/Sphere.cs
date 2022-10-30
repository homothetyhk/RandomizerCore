namespace RandomizerCore.Randomization
{
    /// <summary>
    /// The computational result of a step of the randomizer.
    /// </summary>
    public class Sphere
    {
        /// <summary>
        /// The depth of the sphere.
        /// </summary>
        public int depth;

        /// <summary>
        /// The index of the randomization group of the sphere.
        /// </summary>
        public int groupIndex;

        /// <summary>
        /// The label of the randomization group of the sphere.
        /// </summary>
        public string groupLabel;

        /// <summary>
        /// Indicates whether this is the result of the final step of the stage.
        /// </summary>
        public bool final;

        /// <summary>
        /// Items to be placed in the current sphere or earlier.
        /// </summary>
        public List<IRandoItem> Items;

        /// <summary>
        /// Locations unlocked after placing Items.
        /// </summary>
        public List<IRandoLocation> Locations;
    }
}

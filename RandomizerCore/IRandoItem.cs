namespace RandomizerCore
{
    public interface IRandoItem : ILogicItem
    {
        /// <summary>
        /// Property managed by the randomizer and certain events. Lower priorities are placed first.
        /// </summary>
        public float Priority { get; set; }
        /// <summary>
        /// Property managed by the randomizer for internal tracking.
        /// </summary>
        public State Placed { get; set; }
        /// <summary>
        /// Property managed by the randomizer. Set to the index of the sphere in which the item is placed.
        /// </summary>
        public int Sphere { get; set; }
        /// <summary>
        /// Property managed by the randomizer. Set true if the item is not placed in the last sphere.
        /// </summary>
        public bool Required { get; set; }
    }
}

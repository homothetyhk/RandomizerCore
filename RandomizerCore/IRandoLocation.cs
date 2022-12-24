using RandomizerCore.Logic;

namespace RandomizerCore
{
    /// <summary>
    /// Interface for locations to be managed by the randomizer.
    /// </summary>
    public interface IRandoLocation : ILogicDef
    {
        /// <summary>
        /// Property managed by the randomizer and certain events. Lower priorities are filled first.
        /// </summary>
        public float Priority { get; set; }
        /// <summary>
        /// Property managed by the randomizer for internal tracking.
        /// </summary>
        public TempState Reachable { get; set; }
        /// <summary>
        /// Property managed by the randomizer. Set to the index of the sphere in which the location is first reachable.
        /// </summary>
        public int Sphere { get; set; }
    }

}

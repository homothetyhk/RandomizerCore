using RandomizerCore.Logic;

namespace RandomizerCore
{
    // There is special behavior for IRandoLocations which implement additional interfaces.
    // An IRandoLocation which is an ILogicItem receives waypoint-like treatment in some update entries: when CanGet returns true, it may be added to the pm.
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

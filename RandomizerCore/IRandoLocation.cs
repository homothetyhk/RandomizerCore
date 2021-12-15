using RandomizerCore.Logic;

namespace RandomizerCore
{
    // There is special behavior for IRandoLocations which implement additional interfaces.
    // An IRandoLocation which is an ILogicItem receives waypoint-like treatment in some update entries: when CanGet returns true, it may be added to the pm.
    public interface IRandoLocation : ILogicDef
    {
        public int Priority { get; set; }
        public State Reachable { get; set; }
    }

}

namespace RandomizerCore.Logic
{
    public interface ILogicDef
    {
        string Name { get; }
        bool CanGet(ProgressionManager pm);
        IEnumerable<Term> GetTerms();
    }

    /// <summary>
    /// Interface for locations which have waypoint-like behavior, in that they modify the ProgressionManager when they become reachable.
    /// </summary>
    public interface ILocationWaypoint
    {
        /// <summary>
        /// Returns a logic item which applies an effect when the location becomes reachable.
        /// </summary>
        ILogicItem GetReachableEffect();
    }

}

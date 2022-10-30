using RandomizerCore.Logic;
using RandomizerCore.LogicItems;

namespace RandomizerCore
{
    /// <summary>
    /// Interface used by items and itemlikes (transitions, waypoints).
    /// </summary>
    public interface ILogicItem
    {
        string Name { get; }

        /// <summary>
        /// Directly adds the item to the pm. This does not invoke the pm events, so it is best to implement this explicitly and use pm.Add instead.
        /// </summary>
        void AddTo(ProgressionManager pm);

        /// <summary>
        /// Returns the pm indices potentially modified by the item.
        /// </summary>
        IEnumerable<Term> GetAffectedTerms();
    }

    /// <summary>
    /// Interface which indicates the item has an action once its location is determined.
    /// </summary>
    public interface ILocationDependentItem : ILogicItem
    {
        void Place(ProgressionManager pm, ILogicDef location);
    }

    public interface IRemovableItem
    {
        void RemoveFrom(ProgressionManager pm);
    }

    public abstract record LogicItem(string Name) : ILogicItem, ILogicItemTemplate
    {
        public abstract void AddTo(ProgressionManager pm);

        /// <summary>
        /// Returns the pm indices potentially modified by the item.
        /// </summary>
        public abstract IEnumerable<Term> GetAffectedTerms();

        LogicItem ILogicItemTemplate.Create(LogicManager lm)
        {
            return this;
        }
    }    
}

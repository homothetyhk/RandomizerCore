using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RandomizerCore.Logic;

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

    public interface IRemovableItem
    {
        void RemoveFrom(ProgressionManager pm);
    }

    public abstract record LogicItem(string Name) : ILogicItem
    {
        public abstract void AddTo(ProgressionManager pm);

        /// <summary>
        /// Returns the pm indices potentially modified by the item.
        /// </summary>
        public abstract IEnumerable<Term> GetAffectedTerms();
    }    
}

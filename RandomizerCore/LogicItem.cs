using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RandomizerCore.Logic;

namespace RandomizerCore
{
    public readonly struct LogicItemEffect
    {
        public LogicItemEffect(ItemEffect effect, ILogicManager lm)
        {
            this.id = lm.GetTermIndex(effect.id);
            this.incr = effect.incr;
        }

        public LogicItemEffect(int id, int incr)
        {
            this.id = id;
            this.incr = incr;
        }

        public readonly int id;
        public readonly int incr;
    }

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
        IEnumerable<int> GetAffectedTerms();
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
        public abstract IEnumerable<int> GetAffectedTerms();
    }    
}

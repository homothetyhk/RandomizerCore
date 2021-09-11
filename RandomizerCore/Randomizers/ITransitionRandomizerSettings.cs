using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Randomizers
{
    public interface ITransitionRandomizerSettings : IRandomizerSettings, IItemRandomizerSettings
    {
        /// <summary>
        /// If true, left transitions must map to right transitions, and so on. If false, two-way transitions may map to two-way transitions arbitrarily.
        /// </summary>
        bool Matched { get; }
        /// <summary>
        /// If true, two-way transitions are symmetric. If false, the target of the target of a two-way transition may be different from its source.
        /// </summary>
        bool Coupled { get; }

        List<RandoTransition> GetRandomizedTransitions();
        TransitionPlacementStrategy GetTransitionPlacementStrategy();

        void PostPermuteTransitions(Random rng, IReadOnlyList<RandoTransition> transition);
    }
}

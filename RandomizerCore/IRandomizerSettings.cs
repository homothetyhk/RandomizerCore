using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;
using RandomizerCore.Randomizers;

namespace RandomizerCore
{
    /// <summary>
    /// Base interface for general settings.
    /// </summary>
    public interface IRandomizerSettings
    {
        LogicManager LM { get; }

        void Initialize(Random rng);

        List<ItemPlacement> GetVanillaPlacements();

        void ApplySettings(ProgressionManager pm);

        int Seed { get; }
    }
}

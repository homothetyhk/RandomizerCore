using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Randomizers
{
    [Obsolete]
    public interface IItemRandomizerSettings : IRandomizerSettings
    {
        List<RandoItem> GetRandomizedItems();
        
        List<RandoLocation> GetRandomizedLocations();

        ItemPlacementStrategy GetItemPlacementStrategy();

        void PostPermuteItems(Random rng, IReadOnlyList<RandoItem> items);
        void PostPermuteLocations(Random rng, IReadOnlyList<RandoLocation> locations);
    }
}

using System.Collections.Generic;

namespace RandomizerCore.Logic
{
    public class PrePlacedItemUpdateEntry : UpdateEntry
    {
        public ILogicItem item;
        public ILogicDef location;

        public PrePlacedItemUpdateEntry(ItemPlacement placement) : this(placement.item, placement.location) { }

        public PrePlacedItemUpdateEntry(ILogicItem item, ILogicDef location)
        {
            //Log("Created ppi entry for " + item.name);
            this.item = item;
            this.location = location;
        }

        public override bool CanGet(ProgressionManager pm)
        {
            //Log($"Testing logic for location {location.name}... result: {location.CanGet(pm)}");
            return location.CanGet(pm);
        }

        public override IEnumerable<Term> GetTerms()
        {
            return location.GetTerms();
        }

        public override void OnAdd(ProgressionManager pm)
        {
            //Log("Adding unlocked vanilla item " + item.name);
            pm.Add(item);
        }

        public override void OnRemove(ProgressionManager pm)
        {
            return; // the pm handles removal via restriction
        }

        public override string ToString()
        {
            return $"PPMUE: {item.Name} at {location.Name}";
        }

    }
}

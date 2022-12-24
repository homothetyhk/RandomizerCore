namespace RandomizerCore.Logic
{
    public class PrePlacedItemUpdateEntry : UpdateEntry
    {
        public ILogicItem item;
        public ILogicDef location;

        public PrePlacedItemUpdateEntry(GeneralizedPlacement placement) : this(placement.Item, placement.Location) { }

        public PrePlacedItemUpdateEntry(ILogicItem item, ILogicDef location)
        {
            this.item = item;
            this.location = location;
        }

        public override bool CanGet(ProgressionManager pm)
        {
            return location.CanGet(pm);
        }

        public override IEnumerable<Term> GetTerms()
        {
            return location.GetTerms();
        }

        public override void OnAdd(ProgressionManager pm)
        {
            pm.Add(item);
            if (location is ILocationWaypoint ilw)
            {
                pm.Add(ilw.GetReachableEffect());
            }
            if (item is ILocationDependentItem ildi)
            {
                pm.AddLocationDependentEffect(ildi, location);
            }
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

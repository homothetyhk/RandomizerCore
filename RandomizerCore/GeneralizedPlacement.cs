using RandomizerCore.Logic;

namespace RandomizerCore
{
    public readonly struct GeneralizedPlacement
    {
        public readonly ILogicItem Item;
        public readonly ILogicDef Location;

        public GeneralizedPlacement(ILogicItem Item, ILogicDef Location)
        {
            this.Item = Item;
            this.Location = Location;
        }

        public void Deconstruct(out ILogicItem Item, out ILogicDef Location)
        {
            Item = this.Item;
            Location = this.Location;
        }

        public bool IsRandoPlacement() => Item is IRandoItem && Location is IRandoLocation;
        public RandoPlacement AsRandoPlacement() => new((IRandoItem)Item, (IRandoLocation)Location);

        public static implicit operator GeneralizedPlacement(RandoPlacement p) => new(p.Item, p.Location);
        public static explicit operator RandoPlacement(GeneralizedPlacement p) => new((RandoItem)p.Item, (RandoLocation)p.Location);
    }
}

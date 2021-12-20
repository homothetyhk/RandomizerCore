using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
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

        public bool IsItemPlacement() => Item is RandoItem && Location is RandoLocation;
        public ItemPlacement AsItemPlacement() => new((RandoItem)Item, (RandoLocation)Location);
        public bool IsTransitionPlacement() => Item is RandoTransition && Location is RandoTransition;
        public TransitionPlacement AsTransitionPlacement() => new((RandoTransition)Location, (RandoTransition)Item);
        public bool IsRandoPlacement() => Item is IRandoItem && Location is IRandoLocation;
        public RandoPlacement AsRandoPlacement() => new((IRandoItem)Item, (IRandoLocation)Location);

        public static implicit operator GeneralizedPlacement(ItemPlacement ip) => new(ip.item, ip.location);
        public static implicit operator GeneralizedPlacement(TransitionPlacement tp) => new(tp.target, tp.source);
        public static implicit operator GeneralizedPlacement(RandoPlacement p) => new(p.Item, p.Location);
    }
}

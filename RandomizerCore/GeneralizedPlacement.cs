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
        public bool IsTransitionPlacement() => Item is OldRandoTransition && Location is OldRandoTransition;
        public TransitionPlacement AsTransitionPlacement() => new((OldRandoTransition)Location, (OldRandoTransition)Item);

        public static implicit operator GeneralizedPlacement(ItemPlacement ip) => new(ip.item, ip.location);
        public static implicit operator GeneralizedPlacement(TransitionPlacement tp) => new(tp.target, tp.source);
    }
}

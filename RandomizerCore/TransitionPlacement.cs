using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore
{
    public readonly struct TransitionPlacement
    {
        public readonly OldRandoTransition source;
        public readonly OldRandoTransition target;

        public TransitionPlacement(OldRandoTransition source, OldRandoTransition target)
        {
            this.source = source;
            this.target = target;
        }

        public void Deconstruct(out OldRandoTransition source, out OldRandoTransition target)
        {
            source = this.source;
            target = this.target;
        }

        public override string ToString()
        {
            return $"{source.Name}-->{target.Name}";
        }

    }
}

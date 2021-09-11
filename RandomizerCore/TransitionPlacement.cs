using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore
{
    public readonly struct TransitionPlacement
    {
        public readonly RandoTransition source;
        public readonly RandoTransition target;

        public TransitionPlacement(RandoTransition source, RandoTransition target)
        {
            this.source = source;
            this.target = target;
        }

        public void Deconstruct(out RandoTransition source, out RandoTransition target)
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

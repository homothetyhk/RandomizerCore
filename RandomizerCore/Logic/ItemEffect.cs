using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Logic
{

    public readonly struct ItemEffect
    {
        public ItemEffect(string id, int incr)
        {
            this.id = id;
            this.incr = incr;
        }

        public readonly string id;
        public readonly int incr;
    }

}

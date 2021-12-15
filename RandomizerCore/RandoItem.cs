using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;

namespace RandomizerCore
{
    public class RandoItem : IRandoItem
    {
        public LogicItem item;
        public int Priority { get; set; }

        public string Name => item.Name;

        public State Placed { get ; set ; }

        public void AddTo(ProgressionManager pm)
        {
            item.AddTo(pm);
        }

        public IEnumerable<Term> GetAffectedTerms()
        {
            return item.GetAffectedTerms();
        }

        public override string ToString()
        {
            return Name;
        }
    }
}

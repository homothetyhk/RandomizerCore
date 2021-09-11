using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;

namespace RandomizerCore
{
    public class RandoItem : ILogicItem
    {
        public LogicItem item;
        public int priority;

        public string Name => item.Name;

        public void AddTo(ProgressionManager pm)
        {
            item.AddTo(pm);
        }

        public IEnumerable<int> GetAffectedTerms()
        {
            return item.GetAffectedTerms();
        }

        public override string ToString()
        {
            return Name;
        }
    }
}

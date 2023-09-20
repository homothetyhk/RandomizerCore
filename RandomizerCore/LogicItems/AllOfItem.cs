using RandomizerCore.Logic;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.LogicItems
{
    public sealed record AllOfItem(string Name, IEnumerable<LogicItem> NestedItems) : LogicItem(Name)
    {
        public override void AddTo(ProgressionManager pm)
        {
            foreach (LogicItem item in NestedItems)
            {
                item.AddTo(pm);
            }
        }

        public override IEnumerable<Term> GetAffectedTerms() => NestedItems.SelectMany(i => i.GetAffectedTerms());

        public override bool CheckForEffect(ProgressionManager pm) => NestedItems.Any(i => i.CheckForEffect(pm));
    }
}

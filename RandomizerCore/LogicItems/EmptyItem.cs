using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public record EmptyItem : LogicItem
    {
        public EmptyItem(string Name) : base(Name) { }

        public override void AddTo(ProgressionManager pm)
        {
            return;
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            return Enumerable.Empty<Term>();
        }
    }
}

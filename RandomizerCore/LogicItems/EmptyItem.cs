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

        public override IEnumerable<int> GetAffectedTerms()
        {
            return Enumerable.Empty<int>();
        }
    }

    public class EmptyItemTemplate : LogicItemTemplate
    {
        public override IEnumerable<string> GetItemFlags()
        {
            return Enumerable.Empty<string>();
        }

        public override LogicItem ToLogicItem(ILogicManager lm)
        {
            return new EmptyItem(name);
        }
    }
}

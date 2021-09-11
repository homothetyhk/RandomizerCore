using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public sealed record BranchedItem(string Name, LogicDef Logic, LogicItem TrueItem, LogicItem FalseItem) : LogicItem(Name)
    {
        public override void AddTo(ProgressionManager pm)
        {
            if (Logic.CanGet(pm))
            {
                TrueItem?.AddTo(pm);
            }
            else
            {
                FalseItem?.AddTo(pm);
            }
        }

        public override IEnumerable<int> GetAffectedTerms()
        {
            return (TrueItem?.GetAffectedTerms() ?? Enumerable.Empty<int>())
                .Concat(FalseItem?.GetAffectedTerms() ?? Enumerable.Empty<int>());
        }
    }


    public sealed class BranchedItemTemplate : LogicItemTemplate
    {
        public string logic;
        public LogicItemTemplate trueItem;
        public LogicItemTemplate falseItem;

        public override LogicItem ToLogicItem(ILogicManager lm)
        {
            return new BranchedItem(name, lm.FromString(new RawLogicDef(name, logic)), trueItem.ToLogicItem(lm), falseItem.ToLogicItem(lm));
        }

        public override IEnumerable<string> GetItemFlags()
        {
            return (trueItem?.GetItemFlags() ?? Enumerable.Empty<string>()).Concat(falseItem?.GetItemFlags() ?? Enumerable.Empty<string>());
        }
    }
}

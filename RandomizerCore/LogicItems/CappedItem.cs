﻿using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public sealed record CappedItem(string Name, TermValue[] Effects, TermValue Cap) : LogicItem(Name), IConditionalItem
    {
        public override void AddTo(ProgressionManager pm)
        {
            if (CheckForEffect(pm))
            {
                for (int i = 0; i < Effects.Length; i++)
                {
                    pm.Incr(Effects[i]);
                }
            }
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            return Effects.Select(e => e.Term);
        }

        public bool CheckForEffect(ProgressionManager pm) => !pm.Has(Cap);

        public bool TryAddTo(ProgressionManager pm)
        {
            if (pm.Has(Cap)) return false;
            AddTo(pm);
            return true;
        }
    }
}

using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public sealed record BranchedItem(string Name, LogicDef Logic, LogicItem? TrueItem, LogicItem? FalseItem) : LogicItem(Name), IConditionalItem
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

        public override IEnumerable<Term> GetAffectedTerms()
        {
            return (TrueItem?.GetAffectedTerms() ?? Enumerable.Empty<Term>())
                .Concat(FalseItem?.GetAffectedTerms() ?? Enumerable.Empty<Term>());
        }

        public bool CheckForEffect(ProgressionManager pm)
        {
            if (Logic.CanGet(pm))
            {
                return TrueItem is not null && (TrueItem is not IConditionalItem icl || icl.CheckForEffect(pm));
            }
            else
            {
                return FalseItem is not null && (FalseItem is not IConditionalItem icl || icl.CheckForEffect(pm));
            }
        }

        public bool TryAddTo(ProgressionManager pm)
        {
            if (Logic.CanGet(pm))
            {
                if (TrueItem is null)
                {
                    return false;
                }
                else if (TrueItem is IConditionalItem icl)
                {
                    return icl.TryAddTo(pm);
                }
                else
                {
                    TrueItem.AddTo(pm);
                    return true;
                }
            }
            else
            {
                if (FalseItem is null)
                {
                    return false;
                }
                else if (FalseItem is IConditionalItem icl)
                {
                    return icl.TryAddTo(pm);
                }
                else
                {
                    FalseItem.AddTo(pm);
                    return true;
                }
            }
        }
    }
}

using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public sealed record EmptyItem : LogicItem, IConditionalItem
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

        public bool CheckForEffect(ProgressionManager pm) => false;

        public bool TryAddTo(ProgressionManager pm) => false;
    }
}

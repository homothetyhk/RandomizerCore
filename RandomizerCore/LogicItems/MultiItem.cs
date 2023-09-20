using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public sealed record MultiItem(string Name, TermValue[] Effects) : LogicItem(Name), IRemovableItem
    {
        public override void AddTo(ProgressionManager pm)
        {
            for (int i = 0; i < Effects.Length; i++)
            {
                pm.Incr(Effects[i]);
            }
        }
        public void RemoveFrom(ProgressionManager pm)
        {
            for (int i = 0; i < Effects.Length; i++)
            {
                pm.Incr(Effects[i].Term, -Effects[i].Value);
            }
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            return Effects.Select(e => e.Term);
        }

        public override bool CheckForEffect(ProgressionManager pm) => true;
    }
}

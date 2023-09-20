using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public sealed record SingleItem(string Name, TermValue Effect) : LogicItem(Name), IRemovableItem
    {
        public override void AddTo(ProgressionManager pm)
        {
            pm.Incr(Effect);
        }

        public void RemoveFrom(ProgressionManager pm)
        {
            pm.Incr(Effect.Term, -Effect.Value);
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            yield return Effect.Term;
        }

        public override bool CheckForEffect(ProgressionManager pm) => true;
    }
}

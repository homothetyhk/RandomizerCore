using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems
{
    public record BoolItem(string Name, Term Term) : LogicItem(Name)
    {
        public override void AddTo(ProgressionManager pm)
        {
            pm.Set(Term, 1);
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            yield return Term;
        }
    }
}

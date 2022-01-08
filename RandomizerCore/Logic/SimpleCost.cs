namespace RandomizerCore.Logic
{
    public class SimpleCost : LogicCost
    {
        public Term term;
        public int threshold;

        public SimpleCost(Term term, int amount)
        {
            this.term = term;
            threshold = amount;
        }

        public override bool CanGet(ProgressionManager pm) => pm.Has(term.Id, threshold);
        public override IEnumerable<Term> GetTerms()
        {
            yield return term;
        }

        public override string ToString()
        {
            return $"{{{term.Name} >= {threshold}}}";
        }
    }
}

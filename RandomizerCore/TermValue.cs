using RandomizerCore.Logic;

namespace RandomizerCore
{
    public readonly struct TermValue
    {
        public TermValue(Term Term, int Value)
        {
            this.Term = Term;
            this.Value = Value;
        }

        public override string ToString()
        {
            return $"{Term.Name}: {Value}";
        }

        public readonly Term Term;
        public readonly int Value;
    }
}

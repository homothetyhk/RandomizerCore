using System.Collections;
using System.Collections.ObjectModel;

namespace RandomizerCore.Logic
{
    public class TermCollection : IReadOnlyCollection<Term>
    {
        private readonly Term[][] _terms;
        private readonly Dictionary<string, Term> _termLookup;
        private readonly int[] _counts;
        public readonly ReadOnlyDictionary<string, Term> TermLookup;
        public readonly ReadOnlyCollection<ReadOnlyCollection<Term>> Terms;
        public readonly ReadOnlyCollection<int> Counts;

        public Term this[int id]
        {
            get
            {
                return _terms[(int)Term.GetTermType(id)][Term.GetIndex(id)];
            }
        }

        public bool IsTerm(string term)
        {
            return term is not null && _termLookup.ContainsKey(term);
        }

        public bool IsTerm(string term, TermType type)
        {
            return term is not null && _termLookup.TryGetValue(term, out Term t) && t.Type == type;
        }

        public Term? GetTerm(string term)
        {
            return TermLookup.TryGetValue(term, out Term t) ? t : null;
        }

        public TermCollection(TermCollectionBuilder tcb)
        {
            _count = tcb.Count;
            _terms = tcb.Terms.Select(t => t.ToArray()).ToArray();
            _termLookup = new(tcb.TermLookup);
            _counts = _terms.Select(t => t.Length).ToArray();
            Counts = new(_counts);
            Terms = new(_terms.Select(t => new ReadOnlyCollection<Term>(t)).ToArray());
            TermLookup = new(_termLookup);
        }

        public int Count => _count;
        private readonly int _count;

        public int GetTermCount(TermType type)
        {
            return _terms[(int)type].Length;
        }

        public ReadOnlyCollection<Term> GetTermList(TermType type)
        {
            return Terms[(int)type];
        }

        public IEnumerator<Term> GetEnumerator()
        {
            return _terms.SelectMany(a => a).GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}

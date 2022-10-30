using System.Collections;
using System.Collections.ObjectModel;

namespace RandomizerCore.Logic
{
    public class TermCollectionBuilder : IReadOnlyCollection<Term>
    {
        private readonly List<Term>[] _terms;
        private readonly Dictionary<string, Term> _termLookup;
        public readonly ReadOnlyDictionary<string, Term> TermLookup;
        public readonly ReadOnlyCollection<ReadOnlyCollection<Term>> Terms;

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

        public bool IsDefinable(string term)
        {
            return term is not null && !_termLookup.ContainsKey(term);
        }

        public TermCollectionBuilder()
        {
            _count = 0;
            _terms = new List<Term>[Term.TermTypeCount];
            for (int i = 0; i < _terms.Length; i++) _terms[i] = new();
            _termLookup = new();
            Terms = new(_terms.Select(l => new ReadOnlyCollection<Term>(l)).ToArray());
            TermLookup = new(_termLookup);
        }

        public TermCollectionBuilder(TermCollectionBuilder tcb)
        {
            _count = tcb._count;
            _terms = new List<Term>[tcb._terms.Length];
            for (int i = 0; i < _terms.Length; i++) _terms[i] = new(tcb._terms[i]);
            _termLookup = new(tcb._termLookup);
            Terms = new(_terms.Select(l => new ReadOnlyCollection<Term>(l)).ToArray());
            TermLookup = new(_termLookup);
        }

        public TermCollectionBuilder(TermCollection tc)
        {
            _count = tc.Count;
            _terms = new List<Term>[tc.Terms.Count];
            for (int i = 0; i < _terms.Length; i++) _terms[i] = new(tc.Terms[i]);
            _termLookup = new(tc.TermLookup);
            Terms = new(_terms.Select(l => new ReadOnlyCollection<Term>(l)).ToArray());
            TermLookup = new(_termLookup);
        }


        public Term GetOrAddTerm(string name, TermType type)
        {
            if (TermLookup.TryGetValue(name, out Term t))
            {
                if (t.Type != type) throw new InvalidOperationException($"Cannot create term {name} of type {type}: term has already been defined with type {t.Type}");
                else return t;
            }

            List<Term> l = _terms[(int)type];
            l.Add(t = new(l.Count, name, type));
            _termLookup.Add(name, t);
            _count++;
            return t;
        }


        public int Count => _count;
        private int _count;

        public int GetTermCount(TermType type)
        {
            return _terms[(int)type].Count;
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

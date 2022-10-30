using System.Collections;

namespace RandomizerCore.Logic
{
    public class TermIndexedCollection<T> : IReadOnlyCollection<T>
    {
        private readonly T[][] _ts;

        public static TermIndexedCollection<U> CreatePopulated<U>(TermCollection terms) where U : new()
        {
            TermIndexedCollection<U> us = new(terms);
            for (int i = 0; i < us._ts.Length; i++)
            {
                for (int j = 0; j < us._ts[i].Length; j++)
                {
                    us._ts[i][j] = new();
                }
            }
            return us;
        }

        public TermIndexedCollection(TermCollection terms)
        {
            _ts = new T[Term.TermTypeCount][];
            for (int i = 0; i < _ts.Length; i++) _ts[i] = new T[terms.GetTermCount((TermType)i)];
            Count = terms.Count;
        }

        public T this[int id]
        {
            get
            {
                return _ts[(int)Term.GetTermType(id)][Term.GetIndex(id)];
            }
            set
            {
                _ts[(int)Term.GetTermType(id)][Term.GetIndex(id)] = value;
            }
        }

        public int Count { get; }

        public void PopulateFrom<U>(TermIndexedCollection<U> us, Func<U, T> selector)
        {
            for (int i = 0; i < _ts.Length; i++)
            {
                for (int j = 0; j < _ts[i].Length; j++)
                {
                    _ts[i][j] = selector(us._ts[i][j]);
                }
            }
        }

        public void ZipAction<U>(TermIndexedCollection<U> us, Action<U, T> action)
        {
            for (int i = 0; i < _ts.Length; i++)
            {
                for (int j = 0; j < _ts[i].Length; j++)
                {
                    action(us._ts[i][j], _ts[i][j]);
                }
            }
        }

        public IEnumerator<T> GetEnumerator()
        {
            return _ts.SelectMany(t => t).GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}

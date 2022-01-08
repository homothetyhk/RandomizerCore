namespace RandomizerCore.Extensions
{
    public static class CollectionExtensions
    {
        public static T ArgMin<T>(this IEnumerable<T> ts) where T : IComparable<T>
        {
            IEnumerator<T> e = ts.GetEnumerator();
            e.MoveNext();
            T t = e.Current;
            while (e.MoveNext())
            {
                T u = e.Current;
                if (t.CompareTo(u) < 0)
                {
                    t = u;
                }
            }
            return t;
        }

        public static T ArgMin<T, U>(this IEnumerable<T> ts, Func<T, U> selector) where U : IComparable<U>
        {
            IEnumerator<T> e = ts.GetEnumerator();
            e.MoveNext();
            T t = e.Current;
            U u = selector(t);
            while (e.MoveNext())
            {
                U v = selector(e.Current);
                if (u.CompareTo(v) < 0)
                {
                    u = v;
                    t = e.Current;
                }
            }
            return t;
        }

        public static T ArgMax<T>(this IEnumerable<T> ts) where T : IComparable<T>
        {
            IEnumerator<T> e = ts.GetEnumerator();
            e.MoveNext();
            T t = e.Current;
            while (e.MoveNext())
            {
                T u = e.Current;
                if (t.CompareTo(u) > 0)
                {
                    t = u;
                }
            }
            return t;
        }

        public static T ArgMax<T, U>(this IEnumerable<T> ts, Func<T, U> selector) where U : IComparable<U>
        {
            IEnumerator<T> e = ts.GetEnumerator();
            e.MoveNext();
            T t = e.Current;
            U u = selector(t);
            while (e.MoveNext())
            {
                U v = selector(e.Current);
                if (u.CompareTo(v) > 0)
                {
                    u = v;
                    t = e.Current;
                }
            }
            return t;
        }

        public static int IndexMin<T, U>(this IEnumerable<T> ts, Func<T, U> selector) where U : IComparable<U>
        {
            IEnumerator<T> e = ts.GetEnumerator();
            e.MoveNext();
            int i = 0;
            int j = 0;
            U u = selector(e.Current);
            while (e.MoveNext())
            {
                j++;
                U v = selector(e.Current);
                if (u.CompareTo(v) < 0)
                {
                    u = v;
                    i = j;
                }
            }
            return i;
        }

        public static bool TryPop<T>(this Stack<T> ts, out T t)
        {
            bool flag = ts.Count > 0;
            t = flag ? ts.Pop() : default;
            return flag;
        }

        public static bool TryPeek<T>(this Stack<T> ts, out T t)
        {
            bool flag = ts.Count > 0;
            t = flag ? ts.Peek() : default;
            return flag;
        }

        public static bool TryDequeue<T>(this Queue<T> ts, out T t)
        {
            bool flag = ts.Count > 0;
            t = flag ? ts.Dequeue() : default;
            return flag;
        }

        public static bool TryPeek<T>(this Queue<T> ts, out T t)
        {
            bool flag = ts.Count > 0;
            t = flag ? ts.Peek() : default;
            return flag;
        }

        public static void StableSort<T>(this IList<T> ts) where T : IComparable<T>
        {
            StableSort(ts, (t, u) => t.CompareTo(u));
        }

        public static void StableSort<T>(this IList<T> ts, Comparison<T> comparison)
        {
            KeyValuePair<int, T>[] keys = new KeyValuePair<int, T>[ts.Count];
            for (int i = 0; i < ts.Count; i++) keys[i] = new(i, ts[i]);

            StableComparer<T> comparer = new(comparison);
            Array.Sort(keys, comparer);

            for (int i = 0; i < ts.Count; i++)
            {
                ts[i] = keys[i].Value;
            }
        }

        private class StableComparer<T> : IComparer<KeyValuePair<int, T>>
        {
            public StableComparer(Comparison<T> comparison) => this.comparison = comparison;
            private readonly Comparison<T> comparison;

            public int Compare(KeyValuePair<int, T> x, KeyValuePair<int, T> y)
            {
                int diff = comparison(x.Value, y.Value);
                return diff != 0 ? diff : x.Key - y.Key;
            }
        }
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Extensions
{
    public static class CollectionExtensions
    {
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

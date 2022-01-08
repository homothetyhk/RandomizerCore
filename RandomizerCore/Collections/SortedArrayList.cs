using RandomizerCore.Extensions;
using System.Collections;

namespace RandomizerCore.Collections
{
    /// <summary>
    /// A sorted list which maintains its sort through binary search during list operations.
    /// </summary>
    public class SortedArrayList<T> : IReadOnlyList<T>, ICollection<T> where T : IComparable<T>
    {
        private readonly List<T> ts;

        public int Count => ts.Count;

        public bool IsReadOnly => ((ICollection<T>)ts).IsReadOnly;

        public T this[int index] { get => ts[index]; set => ts[index] = value; }

        public SortedArrayList()
        {
            ts = new();
        }

        public SortedArrayList(int capacity)
        {
            ts = new(capacity);
        }

        public SortedArrayList(IEnumerable<T> input)
        {
            ts = input.ToList();
            ts.StableSort();
        }

        /// <summary>
        /// Inserts the item into the sorted list using binary search.
        /// </summary>
        public void Add(T t)
        {
            ts.Insert(FindInclusiveLowerBound(t), t);
        }

        /// <summary>
        /// Counts the number of elements less than or equal to the input.
        /// </summary>
        public int CountLE(T t)
        {
            return FindExclusiveUpperBound(t);
        }

        /// <summary>
        /// Counts the number of elements less than the input.
        /// </summary>
        public int CountLT(T t)
        {
            return FindInclusiveLowerBound(t);
        }

        /// <summary>
        /// Counts the number of elements greater than or equal to the input.
        /// </summary>
        public int CountGE(T t)
        {
            return Count - CountLT(t);
        }

        /// <summary>
        /// Counts the number of elements greater than the input.
        /// </summary>
        public int CountGT(T t)
        {
            return Count - CountLE(t);
        }

        /// <summary>
        /// Returns the least index such that its entry is greater than or equal to the input, or 0.
        /// </summary>
        public int FindInclusiveLowerBound(T t)
        {
            int lb = 0;
            int ub = ts.Count;

            while (lb < ub)
            {
                int mid = (lb + ub) / 2;

                if (t.CompareTo(ts[mid]) > 0)
                {
                    lb = mid + 1;
                }
                else
                {
                    ub = mid;
                }
            }
            return lb;
        }

        /// <summary>
        /// Returns the least index such that its entry is strictly greater than the input, or Count.
        /// </summary>
        public int FindExclusiveUpperBound(T t)
        {
            int lb = 0;
            int ub = ts.Count;

            while (lb < ub)
            {
                int mid = (lb + ub) / 2;

                if (t.CompareTo(ts[mid]) < 0)
                {
                    ub = mid;
                }
                else
                {
                    lb = mid + 1;
                }
            }
            return lb;
        }

        public void Clear()
        {
            ts.Clear();
        }

        bool ICollection<T>.Contains(T item)
        {
            int i = FindInclusiveLowerBound(item);
            return i < ts.Count && ts[i].CompareTo(item) == 0;
        }

        public void CopyTo(T[] array, int arrayIndex)
        {
            ts.CopyTo(array, arrayIndex);
        }

        public bool Remove(T item)
        {
            int i = FindInclusiveLowerBound(item);
            if (i < ts.Count && ts[i].CompareTo(item) == 0)
            {
                ts.RemoveAt(i);
                return true;
            }

            return false;
        }

        public void RemoveAt(int index)
        {
            ts.RemoveAt(index);
        }

        public T ExtractMax()
        {
            T t = ts[^1];
            ts.RemoveAt(ts.Count - 1);
            return t;
        }

        public bool TryExtractMax(out T t)
        {
            if (ts.Count == 0)
            {
                t = default;
                return false;
            }
            else
            {
                t = ts[^1];
                ts.RemoveAt(ts.Count - 1);
                return true;
            }
        }

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return ((IEnumerable<T>)ts).GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable)ts).GetEnumerator();
        }
    }
}

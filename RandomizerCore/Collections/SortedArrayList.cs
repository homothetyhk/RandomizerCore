using RandomizerCore.Extensions;
using System.Collections;
using System.Diagnostics.CodeAnalysis;

namespace RandomizerCore.Collections
{
    /// <summary>
    /// A sorted list which maintains its sort through binary search during list operations.
    /// </summary>
    public class SortedArrayList<T> : IReadOnlyList<T>, ICollection<T>
    {
        private readonly List<T> ts;

        private readonly IComparer<T> comparer;
        private readonly IEqualityComparer<T> eqComparer;

        public int Count => ts.Count;

        public bool IsReadOnly => ((ICollection<T>)ts).IsReadOnly;

        public T this[int index] { get => ts[Count - 1 - index]; set => ts[Count - 1 - index] = value; }

        public SortedArrayList()
        {
            ts = new();
            comparer = Comparer<T>.Default;
            eqComparer = EqualityComparer<T>.Default;
        }

        public SortedArrayList(IComparer<T> comparer, IEqualityComparer<T> eqComparer)
        {
            ts = new();
            this.comparer = comparer;
            this.eqComparer = eqComparer;
        }

        public SortedArrayList(int capacity)
        {
            ts = new(capacity);
            comparer = Comparer<T>.Default;
            eqComparer = EqualityComparer<T>.Default;
        }

        public SortedArrayList(int capacity, IComparer<T> comparer, IEqualityComparer<T> eqComparer)
        {
            ts = new(capacity);
            this.comparer = comparer;
            this.eqComparer = eqComparer;
        }

        public SortedArrayList(IEnumerable<T> input)
        {
            ts = input.ToList();
            comparer = Comparer<T>.Default;
            eqComparer = EqualityComparer<T>.Default;
            ts.StableSort(comparer);
            ts.Reverse();
        }

        public SortedArrayList(IEnumerable<T> input, IComparer<T> comparer, IEqualityComparer<T> eqComparer)
        {
            ts = input.ToList();
            this.comparer = comparer;
            this.eqComparer = eqComparer;
            ts.StableSort(comparer);
            ts.Reverse();
        }

        /// <summary>
        /// Inserts the item into the sorted list using binary search. Item will appear after all elements which compare equal.
        /// </summary>
        public void Add(T t)
        {
            ts.Insert(Count - FindExclusiveUpperBound(t), t);
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
            return FindInclusiveLowerBound(t, 0, Count);
        }

        /// <summary>
        /// Returns the least index in the range such that its entry is greater than or equal to the input, or lb.
        /// </summary>
        public int FindInclusiveLowerBound(T t, int lb, int ub)
        {
            while (lb < ub)
            {
                int mid = (lb + ub) / 2;

                if (comparer.Compare(t, this[mid]) > 0)
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
            return FindExclusiveUpperBound(t, 0, Count);
        }

        /// <summary>
        /// Returns the least index in the range such that its entry is strictly greater than the input, or ub.
        /// </summary>
        public int FindExclusiveUpperBound(T t, int lb, int ub)
        {
            while (lb < ub)
            {
                int mid = (lb + ub) / 2;

                if (comparer.Compare(t, this[mid]) < 0)
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

        /// <summary>
        /// Uses binary search to find the range in which the element could be found, and then tests each element in the range for equality.
        /// </summary>
        public bool Contains(T item)
        {
            int lb = FindInclusiveLowerBound(item);
            int ub = FindExclusiveUpperBound(item, lb, Count);
            for (int i = lb; i < ub; i++)
            {
                if (eqComparer.Equals(item, this[i]))
                {
                    return true;
                }
            }
            return false;
        }

        public void CopyTo(T[] array, int arrayIndex)
        {
            ts.CopyTo(array, arrayIndex);
            Array.Reverse(array, arrayIndex, ts.Count);
        }

        /// <summary>
        /// Uses binary search to find the range in which the element could be found, and then removes the first element in the range which gives equality, if it exists.
        /// </summary>
        public bool Remove(T item)
        {
            int lb = FindInclusiveLowerBound(item);
            int ub = FindExclusiveUpperBound(item, lb, Count);
            for (int i = lb; i < ub; i++)
            {
                if (eqComparer.Equals(item, this[i]))
                {
                    RemoveAt(i);
                    return true;
                }
            }
            return false;
        }

        public void RemoveAt(int index)
        {
            ts.RemoveAt(Count - 1 - index);
        }

        public T ExtractMin()
        {
            T t = this[0];
            RemoveAt(0);
            return t;
        }

        public bool TryExtractMin([MaybeNullWhen(false)] out T t)
        {
            if (Count == 0)
            {
                t = default;
                return false;
            }
            else
            {
                t = this[0];
                RemoveAt(0);
                return true;
            }
        }

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            for (int i = 0; i < Count; i++) yield return this[i];
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable<T>)this).GetEnumerator();
        }
    }
}

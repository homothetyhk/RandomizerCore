using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.CompilerServices;

namespace RandomizerCore.Collections
{
    /// <summary>
    /// Stable binary min-heap (equal priorities -> first-in first-out)
    /// </summary>
    public class PriorityQueue<T>
    {
        private PriorityEntry[] list;
        int count;
        int version;

        private int _capacity;
        public int Capacity 
        {
            get => _capacity;
            private set
            {
                _capacity = value;
                PriorityEntry[] arr = new PriorityEntry[value];
                Array.Copy(list, arr, list.Length);
                list = arr;
            }
        }

        public int Count => count;


        public PriorityQueue()
        {
            list = new PriorityEntry[4];
            _capacity = list.Length;
        }

        public PriorityQueue(int capacity)
        {
            list = new PriorityEntry[capacity];
            _capacity = capacity;
        }

        public PriorityQueue(IEnumerable<T> ts, Func<T, int> prioritySelector)
        {
            if (ts is ICollection<T> c) list = new PriorityEntry[c.Count];
            else list = new PriorityEntry[4];

            foreach (T t in ts) Enqueue(prioritySelector(t), t);
        }

        public void Clear()
        {
            Array.Clear(list, 0, Count);
            count = 0;
            version = 0;
        }

        public void Enqueue(int priority, T t)
        {
            int i = count;
            EnsureCapacity(++count);
            list[i] = new PriorityEntry(priority, version++, t);
            int p = GetParent(i);

            while (i > 0 && IsLessThan(i, p))
            {
                Swap(i, p);
                i = p;
                p = GetParent(i);
            }
        }

        public bool TryPeek(out int priority, out T t)
        {
            if (count > 0)
            {
                priority = list[0].priority;
                t = list[0].t;
                return true;
            }
            else
            {
                priority = default;
                t = default;
                return false;
            }
        }

        public void UpdateHead(int newPriority)
        {
            if (Count == 0) throw new InvalidOperationException("Priority queue empty");

            list[0] = new(newPriority, list[0].version, list[0].t);

            int p = 0;
            int l; int r;
            while (true)
            {
                l = GetLeftChild(p);
                r = GetRightChild(p);

                if (l >= count) break;
                else if (r >= count)
                {
                    if (IsLessThan(l, p))
                    {
                        Swap(l, p);
                    }
                    break;
                }
                else if (IsLessThan(l, r))
                {
                    if (IsLessThan(l, p))
                    {
                        Swap(l, p);
                        p = l;
                    }
                    else break;
                }
                else
                {
                    if (IsLessThan(r, p))
                    {
                        Swap(r, p);
                        p = r;
                    }
                    else break;
                }
            }
        }

        public void ExtractMin()
        {
            ExtractMin(out _, out _);
        }

        public void ExtractMin(out T t)
        {
            ExtractMin(out _, out t);
        }

        public void ExtractMin(out int priority, out T t)
        {
            if (!TryExtractMin(out priority, out t)) throw new InvalidOperationException("Priority queue empty.");
        }

        public bool TryExtractMin(out int priority, out T t)
        {
            if (count > 0)
            {
                priority = list[0].priority;
                t = list[0].t;

                list[0] = list[--count];
                list[count] = default;
                int p = 0;
                int l; int r;
                while (true)
                {
                    l = GetLeftChild(p);
                    r = GetRightChild(p);

                    if (l >= count) break;
                    else if (r >= count)
                    {
                        if (IsLessThan(l, p))
                        {
                            Swap(l, p);
                        }
                        break;
                    }
                    else if (IsLessThan(l, r))
                    {
                        if (IsLessThan(l, p))
                        {
                            Swap(l, p);
                            p = l;
                        }
                        else break;
                    }
                    else
                    {
                        if (IsLessThan(r, p))
                        {
                            Swap(r, p);
                            p = r;
                        }
                        else break;
                    }
                }

                return true;
            }
            else
            {
                priority = default;
                t = default;
                return false;
            }
        }



        private void EnsureCapacity(int min)
        {
            if (list.Length < min) Capacity = Math.Max(min, 2 * list.Length);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private bool IsLessThan(int i, int p)
        {
            return list[i].priority < list[p].priority || ((list[i].priority == list[p].priority) && list[i].version < list[p].version);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void Swap(int i, int p)
        {
            PriorityEntry temp = list[i];
            list[i] = list[p];
            list[p] = temp;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static int GetParent(int i) => (i - 1) / 2;

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static int GetLeftChild(int i) => 2 * i + 1;

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static int GetRightChild(int i) => 2 * i + 2;

        private readonly struct PriorityEntry : IComparable<PriorityEntry>
        {
            public readonly int priority;
            public readonly int version;
            public readonly T t;

            public PriorityEntry(int priority, int version, T t)
            {
                this.priority = priority;
                this.version = version;
                this.t = t;
            }

            public int CompareTo(PriorityEntry other)
            {
                int diff = priority - other.priority;
                if (diff != 0) return diff;
                return version - other.version;
            }
        }
    }

    
}

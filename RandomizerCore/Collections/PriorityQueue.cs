using System.Runtime.CompilerServices;

namespace RandomizerCore.Collections
{
    /// <summary>
    /// Stable binary min-heap (equal priorities -> first-in first-out)
    /// </summary>
    public class PriorityQueue<TKey, TValue> where TKey : IComparable<TKey>
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

        public PriorityQueue(IEnumerable<TValue> ts, Func<TValue, TKey> prioritySelector)
        {
            if (ts is ICollection<TValue> c) list = new PriorityEntry[c.Count];
            else list = new PriorityEntry[4];

            foreach (TValue t in ts) Enqueue(prioritySelector(t), t);
        }

        public void Clear()
        {
            Array.Clear(list, 0, Count);
            count = 0;
            version = 0;
        }

        public void Enqueue(TKey priority, TValue t)
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

        public bool TryPeek(out TKey priority, out TValue t)
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

        public void UpdateHead(TKey newPriority)
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

        public void ExtractMin(out TValue t)
        {
            ExtractMin(out _, out t);
        }

        public void ExtractMin(out TKey priority, out TValue t)
        {
            if (!TryExtractMin(out priority, out t)) throw new InvalidOperationException("Priority queue empty.");
        }

        public bool TryExtractMin(out TKey priority, out TValue t)
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
            return list[i].CompareTo(list[p]) < 0;
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
            public readonly TKey priority;
            public readonly int version;
            public readonly TValue t;

            public PriorityEntry(TKey priority, int version, TValue t)
            {
                this.priority = priority;
                this.version = version;
                this.t = t;
            }

            public int CompareTo(PriorityEntry other)
            {
                int c = priority.CompareTo(other.priority);
                if (c != 0) return c;
                return version.CompareTo(other.version);
            }
        }
    }

    
}

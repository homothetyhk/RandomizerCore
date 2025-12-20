using RandomizerCore.Extensions;
using System.Collections;

namespace RandomizerCore.Collections
{
    /// <summary>
    /// A stack of distinct items.
    /// </summary>
    public class HashStack<T> : IEnumerable<T>
    {
        private readonly Stack<T> stack;
        private readonly HashSet<T> set;

        public HashStack()
        {
            stack = new Stack<T>();
            set = new HashSet<T>();
        }

        public HashStack(int capacity)
        {
            stack = new Stack<T>(capacity);
#if NETSTANDARD2_0
            set = new HashSet<T>();
#else
            set = new HashSet<T>(capacity);
#endif
        }

        public HashStack(IEnumerable<T> ts)
        {
            set = new HashSet<T>(ts);
            stack = new Stack<T>(set);
        }

        public int Count => set.Count;

        public bool Push(T item)
        {
            if (set.Add(item))
            {
                stack.Push(item);
                return true;
            }
            return false;
        }

        public void Push(IEnumerable<T> ts)
        {
            foreach (T item in ts)
            {
                if (set.Add(item)) stack.Push(item);
            }
        }

        public T Pop()
        {
            T t = stack.Pop();
            set.Remove(t);
            return t;
        }

        public bool TryPop(out T item)
        {
            if (stack.TryPop(out item))
            {
                set.Remove(item);
                return true;
            }
            return false;
        }

        public void Clear()
        {
            set.Clear();
            stack.Clear();
        }

        public bool Contains(T item)
        {
            return set.Contains(item);
        }

        public Stack<T>.Enumerator GetEnumerator()
        {
            return stack.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return stack.GetEnumerator();
        }

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}

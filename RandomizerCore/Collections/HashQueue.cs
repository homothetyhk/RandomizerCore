﻿using System.Collections;
using RandomizerCore.Extensions;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// A queue of distinct items.
    /// </summary>
    public class HashQueue<T> : IEnumerable<T>
    {
        private readonly Queue<T> queue;
        private readonly HashSet<T> set;

        public HashQueue()
        {
            queue = new Queue<T>();
            set = new HashSet<T>();
        }

        public HashQueue(int capacity)
        {
            queue = new Queue<T>(capacity);
#if NETSTANDARD2_0
            set = new HashSet<T>();
#else
            set = new HashSet<T>(capacity);
#endif
        }

        public HashQueue(IEnumerable<T> ts)
        {
            set = new HashSet<T>(ts);
            queue = new Queue<T>(set);
        }

        public int Count => set.Count;

        public bool Enqueue(T item)
        {
            if (set.Add(item))
            {
                queue.Enqueue(item);
                return true;
            }
            return false;
        }

        public void Enqueue(IEnumerable<T> ts)
        {
            foreach (T item in ts)
            {
                if (set.Add(item)) queue.Enqueue(item);
            }
        }

        public bool TryDequeue(out T item)
        {
            if (queue.TryDequeue(out item))
            {
                set.Remove(item);
                return true;
            }
            return false;
        }

        public void Clear()
        {
            set.Clear();
            queue.Clear();
        }

        public bool Contains(T item)
        {
            return set.Contains(item);
        }

        public IEnumerator<T> GetEnumerator()
        {
            return queue.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return queue.GetEnumerator();
        }
    }
}

using System.Runtime.CompilerServices;

namespace RandomizerCore.Collections
{
    /// <summary>
    /// EqualityComparer which checks equality and generates hash codes according to reference equality.
    /// </summary>
    public class ReferenceEqualityComparer<T> : IEqualityComparer<T> where T : class
    {
        private ReferenceEqualityComparer() { }
        public static ReferenceEqualityComparer<T> Instance { get; } = new();

        public bool Equals(T x, T y)
        {
            return ReferenceEquals(x, y);
        }

        public int GetHashCode(T obj)
        {
            return RuntimeHelpers.GetHashCode(obj);
        }
    }
}

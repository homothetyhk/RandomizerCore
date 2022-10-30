using System.Collections;
using System.Runtime.CompilerServices;

namespace RandomizerCore.Collections
{
    /// <summary>
    /// A BitArray with efficient partial order comparison.
    /// </summary>
    public class RCBitArray : IReadOnlyList<bool>
    {
        private readonly uint[] _array;

        public RCBitArray(int length)
        {
            _array = new uint[(length - 1) / 32 + 1];
            Length = length;
        }

        public RCBitArray(RCBitArray array)
        {
            _array = (uint[])array._array.Clone();
            Length = array.Length;
        }

        public int Length { get; }

        public bool this[int i]
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get
            {
                return (_array[i / 32] & (1u << i)) != 0u;
            }
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            set
            {
                if (value)
                {
                    _array[i / 32] |= (1u << i);
                }
                else
                {
                    _array[i / 32] &= ~(1u << i);
                }
            }
        }

        public bool IsAllFalse()
        {
            for (int i = 0; i < _array.Length; i++) if (_array[i] != 0u) return false;
            return true;
        }

        public void SetAllFalse()
        {
            for (int i = 0; i < _array.Length; i++) _array[i] = 0u;
        }

        public bool IsBitwiseLE(RCBitArray other)
        {
            for (int i = 0; i < _array.Length; i++)
            {
                if ((_array[i] & ~other._array[i]) != 0u) return false;
            }
            return true;
        }

        public int BlockCount { get => _array.Length; }

        int IReadOnlyCollection<bool>.Count => Length;

        public uint GetBlock(int i) => _array[i];
        public void SetBlock(int i, uint value) => _array[i] = value;

        public IEnumerator<bool> GetEnumerator()
        {
            for (int i = 0; i < Length; i++) yield return this[i];
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}

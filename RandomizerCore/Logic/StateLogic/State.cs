using RandomizerCore.Collections;

namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Immutable representation of path-dependent accessibility constraints. Consists of bool fields and nonnegative integer fields.
    /// <br/>States are partially ordered via the bool and integer orderings for each field.
    /// </summary>
    public class State : IState
    {
        private readonly RCBitArray _bools;
        private readonly int[] _ints;

        public State(StateBuilder state)
        {
            state.Destruct(out _bools, out _ints);
        }

        public RCBitArray CloneBools() => new(_bools);
        public int[] CloneInts() => (int[])_ints.Clone();

        public bool GetBool(int id) => _bools[id];
        public int GetInt(int id) => _ints[id];

        public bool CouldSetBoolTrue(int id) => !_bools[id];
        public bool CouldIncrement(int id, int incr, int cap) => _ints[id] <= cap - incr;
        public bool CouldSetIntToValue(int id, int value) => _ints[id] <= value;

        public static bool IsComparablyLE(State left, State right)
        {
            if (!left._bools.IsBitwiseLE(right._bools)) return false;
            for (int i = 0; i < left._ints.Length; i++)
            {
                if (left._ints[i] > right._ints[i]) return false;
            }
            return true;
        }

        public bool IsComparablyLE(State other)
        {
            if (!_bools.IsBitwiseLE(other._bools)) return false;
            for (int i = 0; i < _ints.Length; i++)
            {
                if (_ints[i] > other._ints[i]) return false;
            }
            return true;
        }

        
        internal static bool CompareBoolsLE(State left, RCBitArray right) => left._bools.IsBitwiseLE(right);
        internal static bool CompareBoolsGE(State left, RCBitArray right) => right.IsBitwiseLE(left._bools);
        internal static bool CompareIntsLE(State left, int[] right)
        {
            for (int i = 0; i < left._ints.Length; i++)
            {
                if (left._ints[i] > right[i]) return false;
            }
            return true;
        }
        internal static bool CompareIntsGE(State left, int[] right)
        {
            for (int i = 0; i < left._ints.Length; i++)
            {
                if (left._ints[i] < right[i]) return false;
            }
            return true;
        }
    }
}

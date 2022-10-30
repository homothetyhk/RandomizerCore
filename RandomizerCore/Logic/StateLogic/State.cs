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

        public State(StateManager sm) : this(sm.Bools.Count, sm.Ints.Count) { }

        public State(int bools, int ints)
        {
            _bools = new RCBitArray(bools);
            _ints = new int[ints];
            IsZero = true;
        }

        public State(StateBuilder state)
        {
            IsZero = state.IsZero;
            state.Destruct(out _bools, out _ints);
        }

        public bool IsZero { get; }

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
    }
}

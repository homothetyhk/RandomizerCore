using RandomizerCore.Collections;

namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Object used to create <see cref="State"/>.
    /// </summary>
    public class StateBuilder : IState
    {
        private RCBitArray _bools;
        private int[] _ints;

        public StateBuilder(StateManager sm) : this(sm.Bools.Count, sm.Ints.Count) { }

        public StateBuilder(int bools, int ints)
        {
            _bools = new RCBitArray(bools);
            _ints = new int[ints];
        }

        public StateBuilder(State state)
        {
            _bools = state.CloneBools();
            _ints = state.CloneInts();
        }

        public StateBuilder(StateBuilder state)
        {
            _bools = state.CloneBools();
            _ints = state.CloneInts();
        }

        public bool GetBool(int id) => _bools[id];
        public int GetInt(int id) => _ints[id];

        /// <summary>
        /// Sets the specified field.
        /// </summary>
        public void SetBool(int id, bool value)
        {
            _bools[id] = value;
        }

        /// <summary>
        /// Sets the specified field.
        /// </summary>
        public void SetInt(int id, int value)
        {
            _ints[id] = value;
        }

        /// <summary>
        /// If the field is false, sets it true and returns true. Otherwise, returns false.
        /// </summary>
        public bool TrySetBoolTrue(int id)
        {
            if (_bools[id]) return false;
            return _bools[id] = true;
        }

        /// <summary>
        /// Returns false if the increment would cause the field to exceed the max value. Otherwise, performs <see cref="Increment(int, int)"/> and returns true.
        /// </summary>
        public bool TryIncrement(int id, int incr, int maxValue)
        {
            if (_ints[id] > maxValue - incr || _ints[id] + incr < 0) return false;
            Increment(id, incr);
            return true;
        }
        
        /// <summary>
        /// Increments the specified field.
        /// </summary>
        public void Increment(int id, int incr)
        {
            _ints[id] += incr;
        }

        /// <summary>
        /// Returns false if <paramref name="value"/> is less than the field value. Otherwise, performs <see cref="SetInt(int, int)"/> and returns true.
        /// </summary>
        public bool TrySetIntToValue(int id, int value)
        {
            if (_ints[id] > value) return false;
            SetInt(id, value);
            return true;
        }

        public RCBitArray CloneBools() => new(_bools);
        public int[] CloneInts() => (int[])_ints.Clone();

        /// <summary>
        /// Exposes the data of the <see cref="StateBuilder"/>, and renders it incapable of further modification.
        /// </summary>
        /// <exception cref="InvalidOperationException">The StateBuilder has already been destructed.</exception>
        public void Destruct(out RCBitArray bools, out int[] ints)
        {
            if (_bools is null || _ints is null) throw new InvalidOperationException("The StateBuilder has already been destructed.");

            bools = _bools;
            ints = _ints;
            _bools = null!;
            _ints = null!;
        }

        public static bool IsComparablyLE(StateBuilder left, State right)
        {
            if (!State.CompareBoolsGE(right, left._bools)) return false;
            if (!State.CompareIntsGE(right, left._ints)) return false;
            return true;
        }

        public static bool IsComparablyLE(State left, StateBuilder right)
        {
            if (!State.CompareBoolsLE(left, right._bools)) return false;
            if (!State.CompareIntsLE(left, right._ints)) return false;
            return true;
        }

        public static bool IsComparablyLE(StateBuilder left, StateBuilder right)
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
            return IsComparablyLE(this, other);
        }
    }
}

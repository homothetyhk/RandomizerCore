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

        /// <summary>
        /// Returns true if all fields have their default value: false or 0.
        /// </summary>
        public bool IsZero 
        {
            get
            {
                if (!_bools.IsAllFalse()) return false;
                for (int i = 0; i < _ints.Length; i++) if (_ints[i] != 0) return false;
                return true;
            } 
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
        /// Sets the specified field. If the operation would result in a negative value, clamps the field to 0.
        /// </summary>
        public void SetInt(int id, int value)
        {
            _ints[id] = value;
            if (value < 0) _ints[id] = 0;
        }

        /// <summary>
        /// Sets all fields to their default value: false or 0.
        /// </summary>
        public void SetAllZero()
        {
            _bools.SetAllFalse();
            for (int i = 0; i < _ints.Length; i++) _ints[i] = 0;
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
        /// Increments the specified field. If the operation would result in a negative value, clamps the field to 0.
        /// </summary>
        public void Increment(int id, int incr)
        {
            _ints[id] += incr;
            if (_ints[id] < 0) _ints[id] = 0;
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
            _bools = null;
            _ints = null;
        }
    }
}

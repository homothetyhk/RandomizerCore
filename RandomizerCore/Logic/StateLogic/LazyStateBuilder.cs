namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Struct wrapper for a <see cref="State"/> or <see cref="StateBuilder"/>, which delays creating a <see cref="StateBuilder"/> until a mutating operation is requested.
    /// </summary>
    public struct LazyStateBuilder : IState
    {
        /// <summary>
        /// Wraps the State as a LazyStateBuilder, which delays creating a StateBuilder until a mutating operation is requested.
        /// </summary>
        public LazyStateBuilder(State orig)
        {
            this.orig = orig;
            current = null;
        }

        /// <summary>
        /// Clones the StateBuilder and wraps it as a LazyStateBuilder.
        /// </summary>
        public LazyStateBuilder(StateBuilder current)
        {
            this.orig = null;
            this.current = new(current);
        }

        /// <summary>
        /// Clones the other LazyStateBuilder. If the other wraps a StateBuilder, then the inner StateBuilder is also cloned.
        /// </summary>
        public LazyStateBuilder(LazyStateBuilder other)
        {
            this.orig = other.orig;
            if (other.current is not null) this.current = new(other.current);
            else this.current = null;
        }

        private readonly State orig;
        private StateBuilder current;

        public readonly bool GetBool(int id) => current is null ? orig.GetBool(id) : current.GetBool(id);
        public readonly int GetInt(int id) => current is null ? orig.GetInt(id) : current.GetInt(id);

        /// <summary>
        /// Returns true if all fields have their default value: false or 0.
        /// </summary>
        public readonly bool IsZero { get => current is null ? orig.IsZero : current.IsZero; }

        public StateBuilder GetStateBuilder()
        {
            return current ??= new(orig);
        }

        /// <summary>
        /// Sets the specified field.
        /// </summary>
        public void SetBool(int id, bool value)
        {
            if (current is null) GetStateBuilder();
            current.SetBool(id, value);
        }

        /// <summary>
        /// Sets the specified field. If the operation would result in a negative value, clamps the field to 0.
        /// </summary>
        public void SetInt(int id, int value)
        {
            if (current is null) GetStateBuilder();
            current.SetInt(id, value);
        }

        /// <summary>
        /// Increments the specified field. If the operation would result in a negative value, clamps the field to 0.
        /// </summary>
        public void Increment(int id, int incr)
        {
            if (current is null) GetStateBuilder();
            current.Increment(id, incr);
        }

        /// <summary>
        /// Sets all fields to their default value: false or 0.
        /// </summary>
        public void SetAllZero()
        {
            if (current is null)
            {
                if (orig.IsZero) return;
                else GetStateBuilder();
            }
            current.SetAllZero();
        }

        /// <summary>
        /// If the field is false, sets it true and returns true. Otherwise, returns false.
        /// </summary>
        public bool TrySetBoolTrue(int id)
        {
            if (current is null)
            {
                if (orig.GetBool(id)) return false;
                GetStateBuilder();
            }
            return current.TrySetBoolTrue(id);
        }

        /// <summary>
        /// Returns false if the increment would cause the field to exceed the max value. Otherwise, performs <see cref="Increment(int, int)"/> and returns true.
        /// </summary>
        public bool TryIncrement(int id, int incr, int maxValue)
        {
            if (current is null)
            {
                if (orig.GetInt(id) > maxValue - incr) return false;
                GetStateBuilder();
            }
            return current.TryIncrement(id, incr, maxValue);
        }

        /// <summary>
        /// Returns false if <paramref name="value"/> is less than the field value. Otherwise, performs <see cref="SetInt(int, int)"/> and returns true.
        /// </summary>
        public bool TrySetIntToValue(int id, int value)
        {
            if (current is null)
            {
                if (orig.GetInt(id) > value) return false;
                GetStateBuilder();
            }
            return current.TrySetIntToValue(id, value);
        }

        /// <summary>
        /// If the instance wraps a <see cref="StateBuilder"/>, creates a <see cref="State"/> from the builder. If the instance wraps a <see cref="State"/>, returns that state.
        /// </summary>
        public readonly State GetState()
        {
            return current is null ? orig : new(current);
        }
    }
}

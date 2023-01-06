namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Struct wrapper for a <see cref="State"/> or <see cref="StateBuilder"/>, which delays creating a <see cref="StateBuilder"/> until a mutating operation is requested.
    /// Note that this is a mutable struct. Pass it by ref to or return it from methods which perform mutating operations to ensure the effects of those operations are not lost.
    /// </summary>
    public struct LazyStateBuilder : IState
    {
        private IState state;

        /// <summary>
        /// Wraps the State as a LazyStateBuilder, which delays creating a StateBuilder until a mutating operation is requested.
        /// </summary>
        public LazyStateBuilder(State orig)
        {
            this.state = orig;
        }

        /// <summary>
        /// Clones the StateBuilder and wraps it as a LazyStateBuilder.
        /// </summary>
        public LazyStateBuilder(StateBuilder current)
        {
            this.state = new StateBuilder(current);
        }

        /// <summary>
        /// Clones the other LazyStateBuilder. If the other wraps a StateBuilder, then the inner StateBuilder is also cloned.
        /// </summary>
        public LazyStateBuilder(LazyStateBuilder other)
        {
            this.state = other.state is StateBuilder sb ? new StateBuilder(sb) : other.state;
        }

        public readonly bool GetBool(int id) => state.GetBool(id);
        public readonly int GetInt(int id) => state.GetInt(id);

        private StateBuilder Builder
        {
            get
            {
                return state as StateBuilder ?? (StateBuilder)(state = new StateBuilder((State)state));
            }
        }

        /// <summary>
        /// Sets the specified field.
        /// </summary>
        public void SetBool(int id, bool value)
        {
            if (GetBool(id) == value) return;
            Builder.SetBool(id, value);
        }

        /// <summary>
        /// Sets the specified field. If the operation would result in a negative value, clamps the field to 0.
        /// </summary>
        public void SetInt(int id, int value)
        {
            if (GetInt(id) == value) return;
            Builder.SetInt(id, value);
        }

        /// <summary>
        /// Increments the specified field. If the operation would result in a negative value, clamps the field to 0.
        /// </summary>
        public void Increment(int id, int incr)
        {
            Builder.Increment(id, incr);
        }

        /// <summary>
        /// If the field is false, sets it true and returns true. Otherwise, returns false.
        /// </summary>
        public bool TrySetBoolTrue(int id)
        {
            if (GetBool(id)) return false;
            Builder.SetBool(id, true);
            return true;
        }

        /// <summary>
        /// Returns false if the increment would cause the field to exceed the max value. Otherwise, performs <see cref="Increment(int, int)"/> and returns true.
        /// </summary>
        public bool TryIncrement(int id, int incr, int maxValue)
        {
            int amt = GetInt(id);
            if (amt > maxValue - incr) return false;
            Builder.SetInt(id, amt + incr);
            return true;
        }

        /// <summary>
        /// Returns false if <paramref name="value"/> is less than the field value. Otherwise, performs <see cref="SetInt(int, int)"/> and returns true.
        /// </summary>
        public bool TrySetIntToValue(int id, int value)
        {
            if (GetInt(id) > value) return false;
            SetInt(id, value);
            return true;
        }

        /// <summary>
        /// If the instance wraps a <see cref="StateBuilder"/>, creates a <see cref="State"/> from the builder. If the instance wraps a <see cref="State"/>, returns that state.
        /// Since creating a State from a StateBulder is destructive, this method should also be considered destructive, and the LazyStateBuilder should not be subsequently accessed.
        /// </summary>
        public readonly State GetState()
        {
            return state is State s ? s : new State((StateBuilder)state);
        }

        public static bool IsComparablyLE(LazyStateBuilder left, State right)
        {
            return left.state is State s ? State.IsComparablyLE(s, right) : StateBuilder.IsComparablyLE((StateBuilder)left.state, right);
        }

        public static bool IsComparablyLE(LazyStateBuilder left, StateBuilder right)
        {
            return left.state is State s ? StateBuilder.IsComparablyLE(s, right) : StateBuilder.IsComparablyLE((StateBuilder)left.state, right);
        }

        public static bool IsComparablyLE(State left, LazyStateBuilder right)
        {
            return right.state is State s ? State.IsComparablyLE(left, s) : StateBuilder.IsComparablyLE(left, (StateBuilder)right.state);
        }

        public static bool IsComparablyLE(StateBuilder left, LazyStateBuilder right)
        {
            return right.state is State s ? StateBuilder.IsComparablyLE(left, s) : StateBuilder.IsComparablyLE(left, (StateBuilder)right.state);
        }

        public static bool IsComparablyLE(LazyStateBuilder left, LazyStateBuilder right)
        {
            return left.state is State s ? IsComparablyLE(s, right) : IsComparablyLE((StateBuilder)left.state, right);
        }
    }
}

using System.Collections;

namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A collection of mutually incomparable states, representing a disjunction of possible states.
    /// </summary>
    public class StateUnion : IReadOnlyList<State>
    {
        private readonly State[] _states;

        /// <summary>
        /// Does the collection contain the zero state, in which case it absorbs unions.
        /// </summary>
        public bool Absorbing { get; }

        public static StateUnion Empty { get; } = new();

        public StateUnion()
        {
            _states = Array.Empty<State>();
        }

        public StateUnion(State state)
        {
            _states = new State[] { state };
            if (state.IsZero) Absorbing = true;
        }

        public StateUnion(StateUnion states)
        {
            _states = (State[])states._states.Clone();
            Absorbing = states.Absorbing;
        }

        public StateUnion(List<State> states)
        {
            Reduce(states);
            _states = states.ToArray();
            Absorbing = _states.Length == 1 && _states[0].IsZero;
        }

        private StateUnion(State[] states)
        {
            _states = states;
            Absorbing = _states.Length == 1 && _states[0].IsZero;
        }


        public static bool TryUnion(StateUnion left, List<State> states, out StateUnion result)
        {
            int j;
            for (j = 0; j < states.Count; j++)
            {
                bool ge = false;
                for (int i = 0; i < left.Count; i++)
                {
                    if (State.IsComparablyLE(left[i], states[j]))
                    {
                        ge = true;
                        break;
                    }
                }
                if (!ge) break;
            }
            if (j == states.Count)
            {
                result = left;
                return false;
            }
            result = Union(left, states.Skip(j));
            return true;
        }

        public static StateUnion Union(StateUnion left, IEnumerable<State> lazyRight)
        {
            if (left.Absorbing)
            {
                return new(left);
            }

            List<State> right = lazyRight.ToList();
            Reduce(right);
            if (right.Count == 1 && right[0].IsZero)
            {
                return new(right[0]);
            }

            int start = right.Count - 1; // avoid comparing pairs from the same union as they are added to the list
            for (int i = 0; i < left._states.Length; i++)
            {
                bool ge = false;
                for (int j = start; j >= 0; j--)
                {
                    if (State.IsComparablyLE(left._states[i], right[j]))
                    {
                        right.RemoveAt(j);
                        start--;
                        continue;
                    }
                    if (State.IsComparablyLE(right[j], left._states[i]))
                    {
                        ge = true;
                        break;
                    }
                }
                if (!ge)
                {
                    right.Add(left._states[i]);
                }
            }
            return new(right.ToArray());
        }

        /// <summary>
        /// Returns true and outputs a result if the union operation would not result in the left argument.
        /// </summary>
        public static bool TryUnion(StateUnion left, StateUnion right, out StateUnion result)
        {
            if (left.Absorbing)
            {
                result = left;
                return false;
            }
            if (right.Absorbing)
            {
                result = right;
                return true;
            }

            // try to prove that there are no states in right which can be admitted to left
            int j;
            for (j = 0; j < right.Count; j++)
            {
                bool ge = false;
                for (int i = 0; i < left.Count; i++)
                {
                    if (State.IsComparablyLE(left[i], right[j]))
                    {
                        ge = true;
                        break;
                    }
                }
                if (!ge) break;
            }
            if (j == right.Count)
            {
                result = left;
                return false;
            }

            List<State> states = new() { right[j] };
            for (j++; j < right.Count; j++)
            {
                bool ge = false;
                for (int i = 0; i < left.Count; i++)
                {
                    if (State.IsComparablyLE(left[i], right[j]))
                    {
                        ge = true;
                        break;
                    }
                }
                if (!ge) states.Add(right[j]);
            }
            int end = states.Count; // avoid comparing pairs from the same union as they are added to the list
            for (j = 0; j < left.Count; j++)
            {
                bool ge = false;
                for (int i = 0; i < end; i++)
                {
                    if (State.IsComparablyLE(states[i], left[j]))
                    {
                        ge = true;
                        break;
                    }
                }
                if (!ge) states.Add(left[j]);
            }

            result = new(states.ToArray());
            return true;
        }

        public static StateUnion Union(StateUnion left, StateUnion right)
        {
            if (left.Absorbing)
            {
                return new(left);
            }
            if (right.Absorbing)
            {
                return new(right);
            }

            List<State> states = new();
            for (int i = 0; i < left._states.Length; i++)
            {
                bool ge = false;
                for (int j = 0; j < right._states.Length; j++)
                {
                    if (State.IsComparablyLE(right._states[j], left._states[i]))
                    {
                        ge = true;
                        break;
                    }
                }
                if (!ge)
                {
                    states.Add(left._states[i]);
                }
            }
            int end = states.Count; // avoid comparing pairs from the same union as they are added to the list
            for (int i = 0; i < right._states.Length; i++)
            {
                bool ge = false;
                for (int j = 0; j < end; j++)
                {
                    if (State.IsComparablyLE(states[j], right._states[i]))
                    {
                        ge = true;
                        break;
                    }
                }
                if (!ge)
                {
                    states.Add(right._states[i]);
                }
            }

            return new(states.ToArray());
        }

        public static void Reduce(List<State> states)
        {
            for (int i = 0; i < states.Count; i++)
            {
                if (states[i].IsZero)
                {
                    State zero = states[i];
                    states.Clear();
                    states.Add(zero);
                    return;
                }
            }

            for (int i = states.Count - 1; i >= 0; i--)
            {
                for (int j = states.Count - 1; j >= 0; j--)
                {
                    if (i != j && State.IsComparablyLE(states[j], states[i]))
                    {
                        states.RemoveAt(i);
                        break;
                    }
                }
            }
        }

        IEnumerator<State> IEnumerable<State>.GetEnumerator()
        {
            return ((IEnumerable<State>)_states).GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return _states.GetEnumerator();
        }

        public int Count => _states.Length;

        public State this[int index] => _states[index];
    }
}

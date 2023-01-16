using System.Collections;

namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A collection of mutually incomparable states, representing a disjunction of possible states.
    /// </summary>
    public class StateUnion : IReadOnlyList<State>
    {
        private readonly State[] _states;

        public static StateUnion Empty { get; } = new();

        public StateUnion()
        {
            _states = Array.Empty<State>();
        }

        public StateUnion(State state)
        {
            _states = new State[] { state };
        }

        public StateUnion(List<State> states)
        {
            Reduce(states);
            _states = states.ToArray();
        }

        private StateUnion(State[] states)
        {
            _states = states;
        }

        /// <summary>
        /// A partial ordering which holds if the right union represents equal or better progression than the left union.
        /// Returns true if left is null, or else both are not null and there is a map from left to right which takes each state in left to a <see cref="State.IsComparablyLE(State, State)"/> state in right.
        /// Note: this ordering is reversed from the natural ordering induced by the ordering on State.
        /// </summary>
        public static bool IsProgressivelyLE(StateUnion? left, StateUnion? right)
        {
            if (ReferenceEquals(left, right)) return true;
            if (left is null) return true;
            if (right is null) return false;

            for (int i = 0; i < left.Count; i++)
            {
                for (int j = 0; j < right.Count; j++)
                {
                    if (State.IsComparablyLE(right[j], left[i]))
                    {
                        goto continue_outer;
                    }
                }
                return false;
                continue_outer: continue;
            }
            return true;
        }

        /// <summary>
        /// Returns true if the two state unions are equivalent under the partial order of <see cref="IsProgressivelyLE(StateUnion, StateUnion)"/>.
        /// </summary>
        public static bool IsProgressivelyEqual(StateUnion? left, StateUnion? right)
        {
            if (left is null || right is null) return left is null && right is null;
            if (left.Count != right.Count) return false;
            return IsProgressivelyLE(left, right) && IsProgressivelyLE(right, left);
        }

        public static bool TryUnion(StateUnion left, List<State> states, out StateUnion result)
        {
            int j;
            for (j = 0; j < states.Count; j++)
            {
                for (int i = 0; i < left.Count; i++)
                {
                    if (State.IsComparablyLE(left[i], states[j]))
                    {
                        goto continue_outer;
                    }
                }
                break;
                continue_outer: continue;
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
            List<State> right = lazyRight.ToList();
            Reduce(right);

            int start = right.Count - 1; // avoid comparing pairs from the same union as they are added to the list
            for (int i = 0; i < left._states.Length; i++)
            {
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
                        goto continue_outer;
                    }
                }
                right.Add(left._states[i]);
                continue_outer: continue;
            }
            return new(right.ToArray());
        }

        /// <summary>
        /// Returns true and outputs a result if the union operation would not result in the left argument.
        /// </summary>
        public static bool TryUnion(StateUnion left, StateUnion right, out StateUnion result)
        {
            // try to prove that there are no states in right which can be admitted to left
            int j;
            for (j = 0; j < right.Count; j++)
            {
                for (int i = 0; i < left.Count; i++)
                {
                    if (State.IsComparablyLE(left[i], right[j]))
                    {
                        goto continue_outer;
                    }
                }
                break; // right[j] is not GE any state in left
                continue_outer: continue;
            }
            if (j == right.Count)
            {
                result = left;
                return false;
            }

            List<State> states = new() { right[j] };
            for (j++; j < right.Count; j++)
            {
                for (int i = 0; i < left.Count; i++)
                {
                    if (State.IsComparablyLE(left[i], right[j]))
                    {
                        goto continue_outer;
                    }
                }
                states.Add(right[j]);
                continue_outer: continue;
            }
            int end = states.Count; // avoid comparing pairs from the same union as they are added to the list
            for (j = 0; j < left.Count; j++)
            {
                for (int i = 0; i < end; i++)
                {
                    if (State.IsComparablyLE(states[i], left[j]))
                    {
                        goto continue_outer;
                    }
                }
                states.Add(left[j]);
                continue_outer: continue;
            }

            result = new(states.ToArray());
            return true;
        }

        public static StateUnion Union(StateUnion left, StateUnion right)
        {
            List<State> states = new();
            for (int i = 0; i < left._states.Length; i++)
            {
                for (int j = 0; j < right._states.Length; j++)
                {
                    if (State.IsComparablyLE(right._states[j], left._states[i]))
                    {
                        goto continue_outer;
                    }
                }
                states.Add(left._states[i]);
                continue_outer: continue;
            }
            int end = states.Count; // avoid comparing pairs from the same union as they are added to the list
            for (int i = 0; i < right._states.Length; i++)
            {
                for (int j = 0; j < end; j++)
                {
                    if (State.IsComparablyLE(states[j], right._states[i]))
                    {
                        goto continue_outer;
                    }
                }
                states.Add(right._states[i]);
                continue_outer: continue;
            }

            return new(states.ToArray());
        }

        public static void Reduce(List<State> states)
        {
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

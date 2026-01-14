using RandomizerCore.Logic.StateLogic;
using System.Runtime.CompilerServices;

namespace RandomizerCore.Logic
{
    internal class StatePathBuilder
    {
        public StatePathBuilder()
        {
            StateProvider = null;
            TermReqs = [];
            VarReqs = [];
            StateModifiers = [];
            Source = null;
        }

        public string? Source { get; private set; }
        public IStateProvider? StateProvider;
        public readonly List<TermValue> TermReqs;
        public readonly List<LogicInt> VarReqs;
        public readonly List<StateModifier> StateModifiers;
        public Key ToKey() => new(StateProvider, StateModifiers);

        internal readonly struct Key : IEquatable<Key>
        {
            public readonly IStateProvider? stateProvider;
            public readonly List<StateModifier> stateModifiers;

            public Key(IStateProvider? stateProvider, List<StateModifier> stateModifiers)
            {
                this.stateProvider = stateProvider;
                this.stateModifiers = stateModifiers;
            }

            public bool Equals(Key other)
            {
                if (!ReferenceEquals(stateProvider, other.stateProvider) || stateModifiers.Count != other.stateModifiers.Count) return false;
                for (int i = 0; i < stateModifiers.Count; i++) if (!ReferenceEquals(stateModifiers[i], other.stateModifiers[i])) return false;
                return true;
            }

            public override int GetHashCode()
            {
                int hash = RuntimeHelpers.GetHashCode(stateProvider);
                for (int i = 0; i < stateModifiers.Count; i++) hash = (hash << 5) + hash ^ RuntimeHelpers.GetHashCode(stateModifiers[i]);
                return hash;
            }
        }

        public void SetSource(string source)
        {
            Source = source;
        }

        public void Add(Term t, int exLowerBound)
        {
            TermReqs.Add(new(t, exLowerBound + 1)); // incLowerBound
        }

        public void Add(ILogicVariable variable)
        {
            if (variable is StateProvider || variable is Term { Type: TermType.State })
            {
                IStateProvider sp = (IStateProvider)variable;
                if (StateProvider is null)
                {
                    StateProvider = sp; // state providers are NOT added separately as term/var reqs, except in the case of duplicates. Leftmost SP wins.
                    return;
                }
                else
                {
                    WarnDuplicateStateProvider(sp);
                }
            }

            switch (variable)
            {
                case Term t:
                    TermReqs.Add(new(t, 1));
                    break;
                case LogicInt li:
                    VarReqs.Add(li);
                    break;
                case StateModifier sm:
                    StateModifiers.Add(sm);
                    break;
                case StateAccessVariable sav:
                    throw StateAccessModifierException(sav);
                default:
                    throw new NotImplementedException(variable.GetType().Name);
            }
        }

        public void AndWith(StatePathBuilder other)
        {
            if (StateProvider is null && other.StateProvider is not null && StateModifiers.Count > 0)
            {
                WarnMisplacedStateModifier(other.StateProvider, StateModifiers);
            }

            if (other.StateProvider is not null)
            {
                Add(other.StateProvider); // issues warning in case of duplicate SP, in which case other's SP is added to termreqs or varreqs as appropriate
            }

            TermReqs.AddRange(other.TermReqs);
            VarReqs.AddRange(other.VarReqs);
            StateModifiers.AddRange(other.StateModifiers);
        }

        public void CopyTo(StatePathBuilder other)
        {
            other.Source = Source;
            other.StateProvider = StateProvider;
            other.TermReqs.AddRange(TermReqs);
            other.VarReqs.AddRange(VarReqs);
            other.StateModifiers.AddRange(StateModifiers);
        }

        public void Clear()
        {
            Source = null;
            StateProvider = null;
            TermReqs.Clear();
            VarReqs.Clear();
            StateModifiers.Clear();
        }

        private void WarnDuplicateStateProvider(IStateProvider sp)
        {
            Log($"Warning - DNF for {Source} contains a clause with ambiguous state providers: {StateProvider.Name}, {sp.Name}");
        }

        private void WarnMisplacedStateModifier(IStateProvider sp, IEnumerable<StateModifier> sms)
        {
            Log($"Warning - DNF for {Source} contains a clause with state provider {sp.Name} occuring after state modifiers {string.Join(", ", sms.Select(sm => sm.Name))}.");
        }

        private Exception StateAccessModifierException(StateAccessVariable sav)
        {
            return new InvalidOperationException($"Found StateAccessVariable {sav.Name} in logic. Regular logic requires SAVs to be wrapped in a comparison, to avoid common errors with state partial order.");
        }
    }
}

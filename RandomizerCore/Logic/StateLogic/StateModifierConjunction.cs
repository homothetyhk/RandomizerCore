namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Variable which combines multiple state modifiers and ensures that their joint GetValue method is logically equivalent to their joint ModifyState.
    /// May be less performant than a modifier tailored to the specific conjunction.
    /// </summary>
    public class StateModifierConjunction : StateSplittingVariable
    {
        public override string Name { get; }
        public StateVariable[] StateVariables { get; }
        public const string Prefix = "$STCONJ";

        public StateModifierConjunction(string name, StateVariable[] stateVariables)
        {
            Name = name;
            StateVariables = stateVariables;
        }

        public static bool TryMatch(LogicManager lm, string term, out LogicVariable variable)
        {
            if (VariableResolver.TryMatchPrefix(term, Prefix, out string[] parameters))
            {
                StateVariable[] vars = parameters.Select(p => (StateVariable)lm.GetVariable(p)).ToArray();
                foreach (StateVariable var in vars)
                {
                    if (var is not StateModifyingVariable && var is not StateSplittingVariable)
                    {
                        throw new ArgumentException($"Found unknown state variable {var.Name} in StateModifierConjunction {term}.");
                    }
                }
                variable = new StateModifierConjunction(term, vars);
                return true;
            }
            variable = default;
            return false;
        }

        public override IEnumerable<Term> GetTerms()
        {
            return StateVariables.SelectMany(s => s.GetTerms());
        }

        public override int GetValue(object sender, ProgressionManager pm, StateUnion? localState)
        {
            if (localState is null)
            {
                foreach (StateVariable sv in StateVariables) if (sv.GetValue(sender, pm, localState) == FALSE) return FALSE;
                return TRUE;
            }
            
            for (int i = 0; i < localState.Count; i++)
            {
                if (ModifyState(sender, pm, new(localState[i])).Any()) return TRUE;
            }
            return FALSE;
        }

        public override IEnumerable<LazyStateBuilder>? ModifyState(object sender, ProgressionManager pm, LazyStateBuilder state)
        {
            return ModifyStateRecursive(sender, pm, state, 0);
        }

        private IEnumerable<LazyStateBuilder> ModifyStateRecursive(object sender, ProgressionManager pm, LazyStateBuilder state, int index)
        {
            StateVariable sv = StateVariables[index];
            index++;
            if (StateVariables[index] is StateModifyingVariable smv)
            {
                if (smv.ModifyState(sender, pm, ref state))
                {
                    if (index == StateVariables.Length)
                    {
                        return Yield(state);
                    }
                    else
                    {
                        return ModifyStateRecursive(sender, pm, state, index);
                    }
                }
                else
                {
                    return Enumerable.Empty<LazyStateBuilder>();
                }
            }
            else if (StateVariables[index] is StateSplittingVariable ssv)
            {
                if (index == StateVariables.Length)
                {
                    return ssv.ModifyState(sender, pm, state);
                }
                else if (ssv.ModifyState(sender, pm, state) is IEnumerable<LazyStateBuilder> seq)
                {
                    return seq.SelectMany(s => ModifyStateRecursive(sender, pm, s, index));
                }
                else
                {
                    return Enumerable.Empty<LazyStateBuilder>();
                }
            }
            else throw new NotImplementedException();
        }

        private static IEnumerable<LazyStateBuilder> Yield(LazyStateBuilder state)
        {
            yield return state;
        }
    }
}

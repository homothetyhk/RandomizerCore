using RandomizerCore.Logic;
using RandomizerCore.Logic.StateLogic;
using System.Diagnostics.CodeAnalysis;

namespace RandomizerCoreTests.Util
{
    internal class TestVariableResolver : VariableResolver
    {
        public override bool TryMatch(LogicManager lm, string term, [MaybeNullWhen(false)] out LogicVariable variable)
        {
            if (TryMatchPrefix(term, "$I", out string[]? ps) && ps.Length >= 1 && lm.StateManager.FieldLookup.TryGetValue(ps[0], out StateField? sf) && sf is StateInt si)
            {
                variable = new TestStateFieldIncrement(term, si, ps.Length >= 2 && int.TryParse(ps[1], out int amt) ? amt : 1);
                return true;
            }

            return base.TryMatch(lm, term, out variable);
        }

        private class TestStateFieldIncrement : StateModifier
        {
            public TestStateFieldIncrement(string name, StateInt intField, int amount)
            {
                Name = name;
                IntField = intField;
                Amount = amount;
            }

            public override string Name { get; }
            public StateInt IntField { get; }
            public int Amount { get; }

            public override IEnumerable<Term> GetTerms()
            {
                return Enumerable.Empty<Term>();
            }

            public override IEnumerable<LazyStateBuilder> ModifyState(object? sender, ProgressionManager pm, LazyStateBuilder state)
            {
                state.Increment(IntField, Amount);
                yield return state;
            }
        }
    }
}

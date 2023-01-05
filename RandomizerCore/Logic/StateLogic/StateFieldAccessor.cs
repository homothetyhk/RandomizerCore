namespace RandomizerCore.Logic.StateLogic
{
    public class StateFieldAccessor : StateAccessVariable
    {
        public override string Name => Field.Name;
        public StateField Field { get; }

        public StateFieldAccessor(StateField field)
        {
            Field = field;
        }

        public static bool TryMatch(LogicManager lm, string term, out LogicVariable variable)
        {
            if (lm.StateManager.FieldLookup.TryGetValue(term, out StateField field))
            {
                variable = new StateFieldAccessor(field);
                return true;
            }
            variable = default;
            return false;
        }

        public override int GetValue<T>(object? sender, ProgressionManager pm, T state)
        {
            return Field is StateBool ? state.GetBool(Field) ? TRUE : FALSE : state.GetInt(Field);
        }

        public override IEnumerable<Term> GetTerms()
        {
            return Enumerable.Empty<Term>();
        }

        public override IEnumerable<StateField> GetStateFields()
        {
            yield return Field;
        }
    }
}

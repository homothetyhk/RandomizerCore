namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Class which acts as a string-based StateBuilder before the StateManager exists.
    /// </summary>
    public class PreState
    {
        public Dictionary<string, bool> ModifiedBoolFields { get; } = new();
        public Dictionary<string, int> ModifiedIntFields { get; } = new();

        public PreState() { }
        public PreState(State s, StateManager sm)
        {
            foreach (StateBool sb in sm.Bools)
            {
                bool b = s.GetBool(sb);
                if (b != sb.GetDefaultValue(sm))
                {
                    SetBool(sb.Name, b);
                }
            }
            foreach (StateInt si in sm.Ints)
            {
                int i = s.GetInt(si);
                if (i != si.GetDefaultValue(sm))
                {
                    SetInt(si.Name, i);
                }
            }
        }
        public PreState(PreState other)
        {
            ModifiedBoolFields = new(other.ModifiedBoolFields);
            ModifiedIntFields = new(other.ModifiedIntFields);
        }


        public bool GetBool(string fieldName) => ModifiedBoolFields.TryGetValue(fieldName, out bool value) ? value : false;
        public int GetInt(string fieldName) => ModifiedIntFields.TryGetValue(fieldName, out int value) ? value : 0;
        public void SetBool(string fieldName, bool value) => ModifiedBoolFields[fieldName] = value;
        public void SetInt(string fieldName, int value) => ModifiedIntFields[fieldName] = value;

        public State ToState(StateManager sm)
        {
            return new(ToStateBuilder(sm));
        }

        public StateBuilder ToStateBuilder(StateManager sm)
        {
            StateBuilder sb = new(sm);
            foreach (var kvp in ModifiedBoolFields)
            {
                sb.SetBool(sm.GetBoolStrict(kvp.Key), kvp.Value);
            }
            foreach (var kvp in ModifiedIntFields)
            {
                sb.SetInt(sm.GetIntStrict(kvp.Key), kvp.Value);
            }
            return sb;
        }
    }
}

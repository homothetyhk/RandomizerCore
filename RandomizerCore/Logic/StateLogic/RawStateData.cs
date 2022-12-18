namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Serializable form of StateManager/StateManagerBuilder.
    /// </summary>
    public class RawStateData
    {
        public RawStateData() { }
        public RawStateData(StateManager sm)
        {
            Fields = new()
            {
                { StateFieldType.Bool.ToString(), sm.Bools.Select(sb => sb.Name).ToList()  },
                { StateFieldType.Int.ToString(), sm.Ints.Select(si => si.Name).ToList()  },
            };
            Tags = sm.TagLookup.ToDictionary(kvp => kvp.Key, kvp => kvp.Value.Select(sf => sf.Name).ToList());
            Properties = sm.FieldProperties.ToDictionary(kvp => kvp.Key, kvp => kvp.Value.ToDictionary(kvp => kvp.Key, kvp => kvp.Value));
            NamedStates = sm.NamedStates.ToDictionary(kvp => kvp.Key, kvp => new PreState(kvp.Value, sm));
            NamedStateUnions = sm.NamedStateUnions.ToDictionary(kvp => kvp.Key, kvp => kvp.Value.Select(s => new PreState(s, sm)).ToList());
        }

        public Dictionary<string, List<string>> Fields;
        public Dictionary<string, List<string>> Tags;
        public Dictionary<string, Dictionary<string, object?>> Properties;
        public Dictionary<string, PreState> NamedStates;
        public Dictionary<string, List<PreState>> NamedStateUnions;
    }
}

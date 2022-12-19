using System.Collections.ObjectModel;

namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Object used to create <see cref="StateManager"/>.
    /// </summary>
    public class StateManagerBuilder
    {
        public ReadOnlyCollection<StateBool> Bools { get; }
        public ReadOnlyCollection<StateInt> Ints { get; }
        public ReadOnlyDictionary<string, StateField> FieldLookup { get; }
        private readonly List<StateBool> _bools;
        private readonly List<StateInt> _ints;
        private readonly Dictionary<string, StateField> _fieldLookup;

        private readonly Dictionary<string, Dictionary<string, object?>> _properties;
        private readonly Dictionary<string, PreState> _namedStates;
        private readonly Dictionary<string, List<PreState>> _namedStateUnions;


        public StateManagerBuilder()
        {
            _bools = new();
            _ints = new();
            _fieldLookup = new();
            
            _properties = new();
            _namedStates = new();
            _namedStateUnions = new();

            Bools = new(_bools);
            Ints = new(_ints);
            FieldLookup = new(_fieldLookup);
        }

        public StateManagerBuilder(StateManager sm)
        {
            _bools = new(sm.Bools);
            Bools = new(_bools);

            _ints = new(sm.Ints);
            Ints = new(_ints);

            _fieldLookup = new(sm.FieldLookup);
            FieldLookup = new(_fieldLookup);

            _properties = sm.FieldProperties.ToDictionary(kvp => kvp.Key, kvp => kvp.Value.ToDictionary(p => p.Key, p => p.Value));
            _namedStates = sm.NamedStates.ToDictionary(kvp => kvp.Key, kvp => new PreState(kvp.Value, sm));
            _namedStateUnions = sm.NamedStateUnions.ToDictionary(kvp => kvp.Key, kvp => kvp.Value.Select(s => new PreState(s, sm)).ToList());
        }

        public StateManagerBuilder(StateManagerBuilder smb)
        {
            _bools = new(smb.Bools);
            _ints = new(smb.Ints);
            _fieldLookup = new(smb.FieldLookup);
            Bools = new(_bools);
            Ints = new(_ints);
            FieldLookup = new(_fieldLookup);

            _properties = smb._properties.ToDictionary(kvp => kvp.Key, kvp => new Dictionary<string, object?>(kvp.Value));
            _namedStates = smb._namedStates.ToDictionary(kvp => kvp.Key, kvp => new PreState(kvp.Value));
            _namedStateUnions = smb._namedStateUnions.ToDictionary(kvp => kvp.Key, kvp => kvp.Value.Select(s => new PreState(s)).ToList());
        }

        public StateBool GetOrAddBool(string name)
        {
            if (name is null) throw new ArgumentNullException(nameof(name));
            if (_fieldLookup.TryGetValue(name, out StateField field))
            {
                if (field is StateBool sb) return sb;
                else throw new InvalidOperationException($"Cannot define StateBool {name}: name already defines state field of type {field.GetType().Name}");
            }

            StateBool @bool = new(_bools.Count, name);
            _bools.Add(@bool);
            _fieldLookup.Add(name, @bool);
            return @bool;
        }

        public StateInt GetOrAddInt(string name)
        {
            if (name is null) throw new ArgumentNullException(nameof(name));
            if (_fieldLookup.TryGetValue(name, out StateField field))
            {
                if (field is StateInt si) return si;
                else throw new InvalidOperationException($"Cannot define StateInt {name}: name already defines state field of type {field.GetType().Name}.");
            }

            StateInt @int = new(_ints.Count, name);
            _ints.Add(@int);
            _fieldLookup.Add(name, @int);
            return @int;
        }

        public StateField GetOrAddField(string name, StateFieldType type)
        {
            return type switch
            {
                StateFieldType.Bool => GetOrAddBool(name),
                StateFieldType.Int => GetOrAddInt(name),
                _ => throw new NotSupportedException(type.ToString()),
            };
        }

        public bool TryGetProperty(string fieldName, string propertyName, out object? propertyValue)
        {
            if(_properties.TryGetValue(fieldName, out Dictionary<string, object?> propertyLookup) && propertyLookup.TryGetValue(propertyName, out propertyValue))
            {
                return true;
            }
            propertyValue = null;
            return false;
        }

        /// <summary>
        /// Sets the field's property to the specified value.
        /// <br/>Data on the StateManager is expected to be immutable. The generic constraints are to encourage only using primitive types and enums as property values through this overload.
        /// </summary>
        public void SetProperty<T>(string fieldName, string propertyName, T propertyValue) where T : struct, IConvertible 
        {
            if (!_properties.TryGetValue(fieldName, out Dictionary<string, object?> propertyLookup))
            {
                _properties.Add(fieldName, propertyLookup = new());
            }

            propertyLookup[propertyName] = propertyValue;
        }

        /// <summary>
        /// Sets the field's property to the specified value.
        /// </summary>
        public void SetProperty(string fieldName, string propertyName, string propertyValue)
        {
            if (!_properties.TryGetValue(fieldName, out Dictionary<string, object?> propertyLookup))
            {
                _properties.Add(fieldName, propertyLookup = new());
            }

            propertyLookup[propertyName] = propertyValue;
        }

        public void RemoveProperty(string fieldName, string propertyName)
        {
            if (_properties.TryGetValue(fieldName, out Dictionary<string, object?> propertyLookup))
            {
                propertyLookup.Remove(propertyName);
            }
        }

        public IEnumerable<(string, IEnumerable<(string, object?)>)> EnumeratePropertyLists()
        {
            return _properties.Select(kvp => (kvp.Key, kvp.Value.Select(p => (p.Key, p.Value))));
        }
         
        public PreState GetOrAddNamedState(string name)
        {
            if (!_namedStates.TryGetValue(name, out PreState value))
            {
                _namedStates.Add(name, value = new());
            }

            return value;
        }

        public IEnumerable<(string, PreState)> EnumerateNamedStates()
        {
            return _namedStates.Select(kvp => (kvp.Key, kvp.Value));
        }

        public List<PreState> GetOrAddNamedStateUnion(string name)
        {
            if (!_namedStateUnions.TryGetValue(name, out List<PreState> value))
            {
                _namedStateUnions.Add(name, value = new());
            }

            return value;
        }

        public IEnumerable<(string, List<PreState>)> EnumerateNamedStateUnions()
        {
            return _namedStateUnions.Select(kvp => (kvp.Key, kvp.Value));
        }

        public void AppendRawStateData(RawStateData? rsd)
        {
            if (rsd is null) return;

            if (rsd.Fields is not null)
            {
                foreach (var kvp in rsd.Fields)
                {
                    StateFieldType type = (StateFieldType)Enum.Parse(typeof(StateFieldType), kvp.Key, true);
                    foreach (string s in kvp.Value) GetOrAddField(s, type);
                }
            }

            if (rsd.Properties is not null)
            {
                foreach (var kvp in rsd.Properties)
                {
                    if (!_properties.TryGetValue(kvp.Key, out Dictionary<string, object?> propertyLookup))
                    {
                        _properties.Add(kvp.Key, propertyLookup = new());
                    }
                    foreach (var kvp2 in kvp.Value)
                    {
                        propertyLookup[kvp2.Key] = kvp2.Value;
                    }
                }
            }

            if (rsd.NamedStates is not null)
            {
                foreach (var kvp in rsd.NamedStates) _namedStates[kvp.Key] = new(kvp.Value);
            }

            if (rsd.NamedStateUnions is not null)
            {
                foreach (var kvp in rsd.NamedStateUnions) _namedStateUnions[kvp.Key] = new(kvp.Value.Select(s => new PreState(s)));
            }
        }

    }
}

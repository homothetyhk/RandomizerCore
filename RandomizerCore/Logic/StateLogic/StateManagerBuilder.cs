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

        private readonly Dictionary<string, HashSet<StateField>> _tagLookup;
        private readonly Dictionary<string, Dictionary<string, object?>> _properties;
        private readonly Dictionary<string, PreState> _namedStates;
        private readonly Dictionary<string, List<PreState>> _namedStateUnions;


        public StateManagerBuilder()
        {
            _bools = new();
            _ints = new();
            _fieldLookup = new();
            
            _tagLookup = new();
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
            _ints = new(sm.Ints);
            _fieldLookup = new(sm.FieldLookup);
            _tagLookup = sm.TagLookup.ToDictionary(kvp => kvp.Key, kvp => new HashSet<StateField>(kvp.Value));
            Bools = new(_bools);
            Ints = new(_ints);
            FieldLookup = new(_fieldLookup);
        }

        public StateManagerBuilder(StateManagerBuilder smb)
        {
            _bools = new(smb.Bools);
            _ints = new(smb.Ints);
            _fieldLookup = new(smb.FieldLookup);
            _tagLookup = smb._tagLookup.ToDictionary(kvp => kvp.Key, kvp => new HashSet<StateField>(kvp.Value));
            Bools = new(_bools);
            Ints = new(_ints);
            FieldLookup = new(_fieldLookup);
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

        public void DefineOrAppendToTag(string name, string fieldName)
        {
            if (!_tagLookup.TryGetValue(name, out HashSet<StateField> tagList))
            {
                _tagLookup.Add(name, tagList = new());
            }
            tagList.Add(FieldLookup[fieldName]);
        }

        public void DefineOrAppendToTag(string name, IEnumerable<string> fieldNames)
        {
            if (!_tagLookup.TryGetValue(name, out HashSet<StateField> tagList))
            {
                _tagLookup.Add(name, tagList = new());
            }
            tagList.UnionWith(fieldNames.Select(f => FieldLookup[f]));
        }

        public void DefineOrAppendToTag(string name, StateField field)
        {
            if (!_tagLookup.TryGetValue(name, out HashSet<StateField> tagList))
            {
                _tagLookup.Add(name, tagList = new());
            }
            tagList.Add(field);
        }

        public void DefineOrAppendToTag(string name, IEnumerable<StateField> fields)
        {
            if (!_tagLookup.TryGetValue(name, out HashSet<StateField> tagList))
            {
                _tagLookup.Add(name, tagList = new());
            }
            tagList.UnionWith(fields);
        }

        public bool TryGetTaggedFields(string name, out IEnumerable<StateField> taggedFields)
        {
            if (_tagLookup.TryGetValue(name, out HashSet<StateField> fieldSet))
            {
                taggedFields = fieldSet;
                return true;
            }

            taggedFields = null;
            return false;
        }

        public IEnumerable<(string tag, IEnumerable<StateField>)> EnumerateTagLists()
        {
            return _tagLookup.Select(kvp => (kvp.Key, (IEnumerable<StateField>)kvp.Value));
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

        public void SetProperty(string fieldName, string propertyName, object? propertyValue)
        {
            if (!_properties.TryGetValue(fieldName, out Dictionary<string, object?> propertyLookup))
            {
                _properties.Add(fieldName, propertyLookup = new());
            }

            propertyLookup[propertyName] = propertyValue;
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
    }
}

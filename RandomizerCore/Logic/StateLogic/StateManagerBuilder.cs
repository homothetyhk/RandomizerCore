using System.Collections.ObjectModel;

namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Object used to create <see cref="StateManager"/>.
    /// </summary>
    public class StateManagerBuilder
    {
        public readonly ReadOnlyCollection<StateBool> Bools;
        public readonly ReadOnlyCollection<StateInt> Ints;
        public readonly ReadOnlyDictionary<string, StateField> FieldLookup;
        private readonly List<StateBool> _bools;
        private readonly List<StateInt> _ints;
        private readonly Dictionary<string, StateField> _fieldLookup;
        private readonly Dictionary<string, HashSet<StateField>> _tagLookup;

        public StateManagerBuilder()
        {
            _bools = new();
            _ints = new();
            _fieldLookup = new();
            _tagLookup = new();
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

        public StateBool GetOrAddBool(string name, bool defaultValue = false)
        {
            if (name is null) throw new ArgumentNullException(nameof(name));
            if (_fieldLookup.TryGetValue(name, out StateField field))
            {
                if (field is StateBool sb)
                {
                    if (sb.DefaultValue == defaultValue) return sb;
                    else typeof(StateBool).GetProperty(nameof(StateBool.DefaultValue)).SetValue(sb, defaultValue);
                }
                else throw new InvalidOperationException($"Cannot define StateBool {name}: name already defines state field of type {field.GetType().Name}");
            }

            StateBool @bool = new(_bools.Count, name) { DefaultValue = defaultValue };
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

        public StateInt GetOrAddInt(string name, int defaultValue = 0)
        {
            if (name is null) throw new ArgumentNullException(nameof(name));
            if (_fieldLookup.TryGetValue(name, out StateField field))
            {
                if (field is StateInt si && si.DefaultValue == defaultValue)
                {
                    if (si.DefaultValue == defaultValue) return si;
                    else typeof(StateInt).GetProperty(nameof(StateInt.DefaultValue)).SetValue(si, defaultValue);
                }
                else throw new InvalidOperationException($"Cannot define StateInt {name}: name already defines state field of type {field.GetType().Name}.");
            }

            StateInt @int = new(_ints.Count, name) { DefaultValue = defaultValue };
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

        public StateField GetOrAddField(string name, StateFieldType type, object? defaultValue = null)
        {
            return type switch
            {
                StateFieldType.Bool => defaultValue is bool b ? GetOrAddBool(name, b) : GetOrAddBool(name),
                StateFieldType.Int => defaultValue is int i ? GetOrAddInt(name, i) : GetOrAddInt(name),
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

        public ReadOnlyDictionary<string, ReadOnlyCollection<StateField>> GetImmutableTagList()
        {
            return new(_tagLookup.ToDictionary(kvp => kvp.Key, kvp => new ReadOnlyCollection<StateField>(kvp.Value.ToArray())));
        }
    }
}

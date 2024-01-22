using System.Collections.ObjectModel;
using System.Text;

namespace RandomizerCore.Logic.StateLogic
{
    public record StateBool(int Id, string Name) : StateField(Id, Name)
    {
        public bool GetDefaultValue(StateManager sm)
        {
            if (sm.GetProperty(Name, DefaultValuePropertyName) is bool b) return b;
            return false;
        }
        public override StateFieldType GetFieldType()
        {
            return StateFieldType.Bool;
        }
    }
    public record StateInt(int Id, string Name) : StateField(Id, Name)
    {
        public int GetDefaultValue(StateManager sm)
        {
            if (sm.GetProperty(Name, DefaultValuePropertyName) is int i) return i;
            return 0;
        }

        public override StateFieldType GetFieldType()
        {
            return StateFieldType.Int;
        }
    }
    public abstract record StateField(int Id, string Name)
    {
        public const string DefaultValuePropertyName = "DefaultValue";
        public abstract StateFieldType GetFieldType();
        public static implicit operator int(StateField sf) => sf.Id;
    }

    public enum StateFieldType
    {
        Bool,
        Int,
    }

    /// <summary>
    /// Object which manages the list of fields which <see cref="State"/> should represent.
    /// </summary>
    public class StateManager
    {
        public ReadOnlyCollection<StateBool> Bools { get; }
        public ReadOnlyCollection<StateInt> Ints { get; }
        public ReadOnlyDictionary<string, StateField> FieldLookup { get; }
        public ReadOnlyDictionary<string, ReadOnlyDictionary<string, object?>> FieldProperties { get; }
        public ReadOnlyDictionary<string, State> NamedStates { get; }
        public ReadOnlyDictionary<string, StateUnion> NamedStateUnions { get; }
            
        private readonly StateBool[] _bools;
        private readonly StateInt[] _ints;
        private readonly Dictionary<string, StateField> _fieldLookup;

        private readonly Dictionary<string, object> _printer = new();

        public readonly State DefaultState;
        public readonly StateUnion DefaultStateSingleton;
        public readonly StateUnion Empty;

        public StateManager(StateManagerBuilder builder)
        {
            _bools = builder.Bools.ToArray();
            Bools = new(_bools);
            _ints = builder.Ints.ToArray();
            Ints = new(_ints);
            _fieldLookup = new(builder.FieldLookup);
            FieldLookup = new(_fieldLookup);
            FieldProperties = new(builder.EnumeratePropertyLists().ToDictionary(p => p.Item1, p => new ReadOnlyDictionary<string, object?>(p.Item2.ToDictionary(q => q.Item1, q => q.Item2))));
            NamedStates = new(builder.EnumerateNamedStates().ToDictionary(p => p.Item1, p => p.Item2.ToState(this)));
            NamedStateUnions = new(builder.EnumerateNamedStateUnions().ToDictionary(p => p.Item1, p => new StateUnion(p.Item2.Select(s => s.ToState(this)).ToList())));
            DefaultState = CreateDefault();
            DefaultStateSingleton = new(DefaultState);
            Empty = StateUnion.Empty;
        }

        /// <summary>
        /// Fetches the state bool by name. Returns null if not defined.
        /// </summary>
        public StateBool? GetBool(string name)
        {
            FieldLookup.TryGetValue(name, out StateField sf);
            return sf as StateBool;
        }

        /// <summary>
        /// Fetches the state bool by name.
        /// </summary>
        /// <exception cref="ArgumentException">The state bool is not defined.</exception>
        public StateBool GetBoolStrict(string name)
        {
            return GetBool(name) ?? throw new ArgumentException($"StateBool {name} is not defined.");
        }

        /// <summary>
        /// Fetches the state int by name. Returns null if not defined.
        /// </summary>
        public StateInt? GetInt(string name)
        {
            FieldLookup.TryGetValue(name, out StateField sf);
            return sf as StateInt;
        }

        /// <summary>
        /// Fetches the state int by name.
        /// </summary>
        /// <exception cref="ArgumentException">The state int is not defined.</exception>
        public StateInt GetIntStrict(string name)
        {
            return GetInt(name) ?? throw new ArgumentException($"StateInt {name} is not defined.");
        }

        /// <summary>
        /// Returns the value of the property for the field, if defined, or else null.
        /// </summary>
        public object? GetProperty(string fieldName, string propertyName)
        {
            if (FieldProperties.TryGetValue(fieldName, out ReadOnlyDictionary<string, object?> properties)
                && properties.TryGetValue(propertyName, out object? value)) return value;
            return null;
        }

        /// <summary>
        /// Returns true if the property for the field is defined and not null.
        /// </summary>
        public bool HasProperty(string fieldName, string propertyName)
        {
            return GetProperty(fieldName, propertyName) is not null;
        }

        public State? GetNamedState(string name)
        {
            return NamedStates.TryGetValue(name, out State s) ? s : null;
        }

        public State GetNamedStateStrict(string name)
        {
            return GetNamedState(name) ?? throw new ArgumentException($"Named state {name} is not defined.");
        }

        public StateUnion? GetNamedStateUnion(string name)
        {
            return NamedStateUnions.TryGetValue(name, out StateUnion s) ? s : null;
        }

        public StateUnion GetNamedStateUnionStrict(string name)
        {
            return GetNamedStateUnion(name) ?? throw new ArgumentException($"Named state union {name} is not defined.");
        }

        public string PrettyPrint<T>(T state) where T : IState
        {
            StringBuilder sb = new();
            return PrettyPrint<T>(state, sb).ToString();
        }

        private StringBuilder PrettyPrint<T>(T state, StringBuilder sb) where T : IState
        {
            sb.Append('{');
            for (int i = 0; i < Bools.Count; i++)
            {
                if (state.GetBool(i))
                {
                    sb.Append(Bools[i].Name).Append(":T,");
                }
            }
            for (int i = 0; i < Ints.Count; i++)
            {
                int j = state.GetInt(i);
                if (j != 0)
                {
                    sb.Append(Ints[i].Name).Append(':').Append(j).Append(',');
                }
            }
            if (sb[^1] == ',') sb[^1] = '}';
            else sb.Append('}');
            return sb;
        }

        public string PrettyPrint(StateUnion? states)
        {
            if (states is null) return "null";
            StringBuilder sb = new();
            sb.Append('[');
            foreach (State state in states)
            {
                PrettyPrint(state, sb).Append(',');
            }
            if (sb[^1] == ',') sb[^1] = ']';
            else sb.Append(']');
            return sb.ToString();
        }

        public Dictionary<string, List<string>> GetFieldDefs()
        {
            return new Dictionary<string, List<string>>()
            {
                { StateFieldType.Bool.ToString(), new(Bools.Select(sb => sb.Name)) },
                { StateFieldType.Int.ToString(), new(Ints.Select(si => si.Name)) }
            };
        }

        private State CreateDefault()
        {
            StateBuilder sb = new(this);
            for (int i = 0; i < _bools.Length; i++)
            {
                if (Bools[i].GetDefaultValue(this))
                {
                    sb.SetBool(i, true);
                }
            }
            for (int i = 0; i < _ints.Length; i++)
            {
                int j = Ints[i].GetDefaultValue(this);
                if (j != 0)
                {
                    sb.SetInt(i, Ints[i].GetDefaultValue(this));
                }
            }
            return new(sb);
        }

    }
}

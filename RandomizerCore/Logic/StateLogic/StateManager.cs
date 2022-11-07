using RandomizerCore.Json;
using System.Collections.ObjectModel;

namespace RandomizerCore.Logic.StateLogic
{
    public record StateBool(int Id, string Name) : StateField(Id, Name)
    {
        public bool DefaultValue { get; init; }
        public override StateFieldType GetFieldType()
        {
            return StateFieldType.Bool;
        }
    }
    public record StateInt(int Id, string Name) : StateField(Id, Name)
    {
        public int DefaultValue { get; init; }
        public override StateFieldType GetFieldType()
        {
            return StateFieldType.Int;
        }
    }
    public abstract record StateField(int Id, string Name)
    {
        public RawStateField ToRawStateDef() => new(Name, GetFieldType());
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
        public readonly ReadOnlyCollection<StateBool> Bools;
        public readonly ReadOnlyCollection<StateInt> Ints;
        public readonly ReadOnlyDictionary<string, StateField> FieldLookup;
        public readonly ReadOnlyDictionary<string, ReadOnlyCollection<StateField>> TagLookup;
        private readonly StateBool[] _bools;
        private readonly StateInt[] _ints;
        private readonly Dictionary<string, StateField> _fieldLookup;

        private readonly Dictionary<string, object> _printer = new();

        public readonly State StartState;
        public readonly StateUnion StartStateSingleton;
        public readonly StateUnion Empty;

        public StateManager(StateManagerBuilder builder)
        {
            _bools = builder.Bools.ToArray();
            _ints = builder.Ints.ToArray();
            _fieldLookup = new(builder.FieldLookup);
            TagLookup = builder.GetImmutableTagList();
            Bools = new(_bools);
            Ints = new(_ints);
            FieldLookup = new(_fieldLookup);
            StartState = CreateDefault();
            StartStateSingleton = new(StartState);
            Empty = StateUnion.Empty;
        }

        public StateBool? GetBool(string name)
        {
            FieldLookup.TryGetValue(name, out StateField sf);
            return sf as StateBool;
        }

        public StateInt? GetInt(string name)
        {
            FieldLookup.TryGetValue(name, out StateField sf);
            return sf as StateInt;
        }

        public IEnumerable<StateField> GetListByTag(string tag)
        {
            TagLookup.TryGetValue(tag, out ReadOnlyCollection<StateField> value);
            return value ?? Enumerable.Empty<StateField>();
        }

        public string PrettyPrint(State state)
        {
            _printer.Clear();
            for (int i = 0; i < Bools.Count; i++)
            {
                if (state.GetBool(i)) _printer.Add(_bools[i].Name, true);
            }
            for (int i = 0; i < Ints.Count; i++)
            {
                int j = state.GetInt(i);
                if (j > 0) _printer.Add(_ints[i].Name, j);
            }
            return JsonUtil.SerializeNonindented(_printer);
        }

        public string PrettyPrint(StateUnion? states)
        {
            if (states is null) return "null";
            return JsonUtil.SerializeNonindented(states.Select(s => PrettyPrint(s))).Replace("\"", "").Replace("\\", "");
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
            for (int i = 0; i < _bools.Length; i++) if (_bools[i].DefaultValue) sb.SetBool(i, true);
            for (int i = 0; i < _ints.Length; i++) if (_ints[i].DefaultValue != 0) sb.SetInt(i, _ints[i].DefaultValue);
            return new(sb);
        }

    }
}

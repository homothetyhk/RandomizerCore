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

        public StateManagerBuilder()
        {
            _bools = new();
            _ints = new();
            _fieldLookup = new();
            Bools = new(_bools);
            Ints = new(_ints);
            FieldLookup = new(_fieldLookup);
        }

        public StateManagerBuilder(StateManager sm)
        {
            _bools = new(sm.Bools);
            _ints = new(sm.Ints);
            _fieldLookup = new(sm.FieldLookup);
            Bools = new(_bools);
            Ints = new(_ints);
            FieldLookup = new(_fieldLookup);
        }

        public StateManagerBuilder(StateManagerBuilder smb)
        {
            _bools = new(smb.Bools);
            _ints = new(smb.Ints);
            _fieldLookup = new(smb.FieldLookup);
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
                else throw new InvalidOperationException($"Cannot define StateInt {name}: name already defines state field of type {field.GetType().Name}");
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
    }
}

using System.Runtime.CompilerServices;

namespace RandomizerCore.Logic
{
    public enum TermType
    {
        Byte,
        Int,
        State
    }

    public class Term
    {
        private const int bitFilter = 0b111_0000_0000_0000_0000_0000_0000_0000;
        private const int indexBits = 28;
        public static int TermTypeCount { get; } = 3;

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static int GetIndex(int id)
        {
            return id & ~bitFilter;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TermType GetTermType(int id)
        {
            return (TermType)(id >> indexBits);
        }

        private static int GetTypeMask(TermType type)
        {
            return ((int)type) << 28;
        }

        // TODO: Figure out how to make old terms default to int, but new terms default to byte?

        public Term(int Id, string Name, TermType Type)
        {
            if (Id < 0) throw new ArgumentOutOfRangeException(nameof(Id));
            this.Id = Id | GetTypeMask(Type);
            this.Name = Name ?? throw new ArgumentNullException(nameof(Name));
            this.Type = Type;
        }

        public readonly int Id;
        public readonly string Name;
        public readonly TermType Type;
        public override string ToString() => Name;

        public static implicit operator int(Term term)
        {
            return term.Id;
        }
    }
}

using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Logic
{
    public enum TermType
    {
        SignedByte,
        Int,
        State
    }

    public class Term : ILogicInt, IStateProvider
    {
        private const int bitFilter = 0b111_0000_0000_0000_0000_0000_0000_0000;
        private const int indexBits = 28;
        public static int TermTypeCount { get; } = 3;

        public static int GetIndex(int id)
        {
            return id & ~bitFilter;
        }

        public static TermType GetTermType(int id)
        {
            return (TermType)(id >> indexBits);
        }

        private static int GetTypeMask(TermType type)
        {
            return ((int)type) << 28;
        }

        public Term(int Id, string Name, TermType Type)
        {
            if (Id < 0) throw new ArgumentOutOfRangeException(nameof(Id));
            this.Id = Id | GetTypeMask(Type);
            this.Name = Name ?? throw new ArgumentNullException(nameof(Name));
            this.Type = Type;
            this.Index = GetIndex(Id);
        }

        /// <summary>
        /// A unique int identifier for the term. The index and type of the term can be recovered from the id using static methods on Term.
        /// </summary>
        public readonly int Id;
        /// <summary>
        /// The name of the term.
        /// </summary>
        public readonly string Name;
        /// <summary>
        /// The type of the term.
        /// </summary>
        public readonly TermType Type;
        /// <summary>
        /// The index of the term, among terms of its type. Terms of different type may share the same index.
        /// </summary>
        public readonly int Index;

        public override string ToString() => Name;

        string ILogicVariable.Name => Name;

        int ILogicInt.GetValue(object? sender, ProgressionManager pm)
        {
            return pm.Get(this);
        }

        IEnumerable<Term> ILogicVariable.GetTerms()
        {
            yield return this;
        }

        StateUnion? IStateProvider.GetInputState(object? sender, ProgressionManager pm)
        {
            return pm.GetState(this);
        }

        public static implicit operator int(Term term)
        {
            return term.Id;
        }
    }
}

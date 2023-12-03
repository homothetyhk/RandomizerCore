namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A <see cref="StateProvider"/> which provides a fixed state union.
    /// </summary>
    public sealed class ConstantStateProvider : StateProvider
    {
        public ConstantStateProvider(string name, StateUnion? value)
        {
            Name = name;
            Value = value;
        }

        public override string Name { get; }
        public StateUnion? Value { get; }

        public override StateUnion? GetInputState(object? sender, ProgressionManager pm)
        {
            return Value;
        }

        public override IEnumerable<Term> GetTerms()
        {
            return Enumerable.Empty<Term>();
        }
    }
}

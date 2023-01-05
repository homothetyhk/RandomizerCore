namespace RandomizerCore.Logic
{
    /// <summary>
    /// A <see cref="LogicVariable"/> which produces an int value.
    /// </summary>
    public abstract class LogicInt : LogicVariable
    {
        public abstract int GetValue(object? sender, ProgressionManager pm);
    }

    /// <summary>
    /// A <see cref="LogicInt"/> which produces a constant value.
    /// </summary>
    public sealed class ConstantInt : LogicInt
    {
        public ConstantInt(int value) => this.value = value;

        public readonly int value;

        public override string Name => value.ToString();
        public override int GetValue(object? sender, ProgressionManager pm) => value;
        public override IEnumerable<Term> GetTerms() => Enumerable.Empty<Term>();
    }

    internal sealed class ConstantBool : LogicInt
    {
        public override string Name { get; }
        public readonly bool value;

        public ConstantBool(string name, bool value)
        {
            Name = name;
            this.value = value;
        }

        public override int GetValue(object? sender, ProgressionManager pm) => value ? TRUE : FALSE;
        public override IEnumerable<Term> GetTerms() => Enumerable.Empty<Term>();
    }
}

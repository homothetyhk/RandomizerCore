namespace RandomizerCore.Logic
{
    public abstract class LogicInt
    {
        /// <summary>
        /// The name of the variable. Should match its usage in logic.
        /// </summary>
        public abstract string Name { get; }
        public abstract int GetValue(object sender, ProgressionManager pm);
        public abstract IEnumerable<Term> GetTerms();
        public override string ToString() => Name;
    }

    public class ConstantInt : LogicInt
    {
        public ConstantInt(int value) => this.value = value;

        public readonly int value;

        public override string Name => value.ToString();
        public override int GetValue(object sender, ProgressionManager pm) => value;
        public override IEnumerable<Term> GetTerms() => Enumerable.Empty<Term>();
    }
}

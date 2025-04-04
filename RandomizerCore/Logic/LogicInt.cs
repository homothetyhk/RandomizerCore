namespace RandomizerCore.Logic
{
    /// <summary>
    /// A <see cref="LogicVariable"/> which produces an int value.
    /// </summary>
    public abstract class LogicInt : LogicVariable, ILogicInt
    {
        public abstract int GetValue(object? sender, ProgressionManager pm);
    }

    public interface ILogicInt : ILogicVariable
    {
        public int GetValue(object? sender, ProgressionManager pm);
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

    internal sealed class ComparisonVariable : LogicInt
    {
        private readonly ILogicInt Left;
        private readonly ILogicInt Right;
        private readonly int Op;

        public ComparisonVariable(string name, ILogicInt left, ILogicInt right, int op)
        {
            Name = name;
            Left = left;
            Right = right;
            Op = op;
        }

        public override string Name { get; }
        public override int GetValue(object? sender, ProgressionManager pm)
        {
            int l = Left.GetValue(sender, pm);
            int r = Right.GetValue(sender, pm);
            int c = l.CompareTo(r);

            return Op switch
            {
                > 0 => c > 0,
                0 => c == 0,
                < 0 => c < 0
            } ? TRUE : FALSE;
        }
        public override IEnumerable<Term> GetTerms()
        {
            return Left.GetTerms().Concat(Right.GetTerms());
        }
    }

    internal sealed class LogicDefVariable : LogicInt
    {
        private readonly LogicDef logic;
        private readonly bool projected;

        /// <param name="logic"></param>
        /// <param name="projected">Indicates whether the variable name should include a projection operator. No effect on behavior.</param>
        public LogicDefVariable(LogicDef logic, bool projected = false)
        {
            this.logic = logic;
            this.projected = projected;
            this.Name = $"*{logic.Name}{(projected ? "/" : "")}";
        }

        public override string Name { get; }
        
        public override int GetValue(object? sender, ProgressionManager pm)
        {
            return logic.CanGet(pm) ? TRUE : FALSE;
        }

        public override IEnumerable<Term> GetTerms()
        {
            return logic.GetTerms();
        }
    }
}
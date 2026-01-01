using RandomizerCore.Logic.StateLogic;

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

    internal sealed class ComparisonVariable(ILogicInt left, ILogicInt right, int op) : LogicInt, IComparisonVariable
    {
        public ILogicInt Left { get; } = left;
        public ILogicInt Right { get; } = right;
        public int Op { get; } = op;
        ILogicVariable IComparisonVariable.Left => Left;
        ILogicVariable IComparisonVariable.Right => Right;

        public override string Name
        {
            get
            {
                string op = Op switch
                {
                    > 0 => ">",
                    < 0 => "<",
                    0 => "="
                };
                return $"{Left.Name}{op}{Right.Name}";
            }
        }

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

    internal sealed class SAVComparisonVariable(StateAccessVariable left, StateAccessVariable right, int op) : StateAccessVariable, IComparisonVariable
    {
        public StateAccessVariable Left { get; } = left;
        public StateAccessVariable Right { get; } = right;
        public int Op { get; } = op;
        ILogicVariable IComparisonVariable.Left => Left;
        ILogicVariable IComparisonVariable.Right => Right;

        public override string Name
        {
            get
            {
                string op = Op switch
                {
                    > 0 => ">",
                    < 0 => "<",
                    0 => "="
                };
                return $"{Left.Name}{op}{Right.Name}";
            }
        }

        public override int GetValue<T>(object? sender, ProgressionManager pm, T state)
        {
            int l = Left.GetValue(sender, pm, state);
            int r = Right.GetValue(sender, pm, state);
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

        public override IEnumerable<StateField> GetStateFields()
        {
            return Left.GetStateFields().Concat(Right.GetStateFields());
        }
    }

    internal sealed class ProjectedStateProvider : LogicInt
    {
        internal readonly IStateProvider stateProvider;

        public ProjectedStateProvider(IStateProvider stateProvider)
        {
            this.stateProvider = stateProvider;
        }

        public override string Name => stateProvider.Name + '/';

        public override IEnumerable<Term> GetTerms()
        {
            return stateProvider.GetTerms();
        }

        public override int GetValue(object? sender, ProgressionManager pm)
        {
            return stateProvider.GetInputState(this, pm) is StateUnion s ? TRUE : FALSE;
        }
    }

    internal sealed class LogicDefVariable : LogicInt
    {
        internal readonly LogicDef logic;
        internal readonly bool projected;
        internal readonly bool reference;

        /// <param name="logic"></param>
        /// <param name="projected">Controls how the variable name is printed. No effect on behavior.</param>
        /// <param name="reference">Controls how the variable name is printed. No effect on behavior.</param>
        public LogicDefVariable(LogicDef logic, bool projected = false, bool reference = false)
        {
            this.logic = logic;
            this.projected = projected;
            this.reference = reference;
        }

        public override string Name 
        { 
            get 
            {
                if (reference)
                {
                    return '*' + logic.Name + (projected ? "/" : "");
                }
                else
                {
                    return projected ? $"({logic.InfixSource})/" : logic.InfixSource;
                }
            }
        }
        
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
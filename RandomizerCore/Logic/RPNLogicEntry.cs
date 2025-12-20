using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Logic
{
    internal readonly struct RPNLogicEntry
    {
        public readonly ILogicVariable? Variable;
        public readonly int Value; // an inclusive-lower bound, if variable is int-valued. For null variable, an operator code.

        public RPNLogicEntry() { }

        public RPNLogicEntry(ILogicVariable? variable) : this(variable, 1) { }

        public RPNLogicEntry(ILogicVariable? variable, int value)
        {
            Variable = variable;
            Value = value;
        }

        public RPNLogicEntry(string name, LogicManager lm)
        {
            if (lm.GetTerm(name) is Term t)
            {
                Variable = t;
            }
            else
            {
                Variable = lm.GetVariableStrict(name);
            }
            Value = 1;
        }

        public RPNLogicEntry(string name, int strictLowerBound, LogicManager lm)
        {
            if (lm.GetTerm(name) is Term t)
            {
                Variable = t;
            }
            else
            {
                Variable = lm.GetVariableStrict(name);
            }
            Value = strictLowerBound + 1;
        }

        public bool IsAnd { get => Variable is null && Value == (int)LogicOperators.AND; }
        public bool IsOr { get => Variable is null && Value == (int)LogicOperators.OR; }
        public bool IsConstTrue { get => Variable is null && Value == (int)LogicOperators.ANY; }
        public bool IsConstFalse { get => Variable is null && Value == (int)LogicOperators.NONE; }

        public bool IsTerm { get => Variable is Term; }
        public bool IsVar { get => Variable is LogicVariable; }
        public bool IsLogicInt { get => Variable is LogicInt; }
        public bool IsStateProvider { get => Variable is IStateProvider && (Variable is not Term t || t.Type == TermType.State); }
        public bool IsStateModifier { get => Variable is StateModifier; }

        public static RPNLogicEntry And { get => new(null, (int)LogicOperators.AND); }
        public static RPNLogicEntry Or { get => new(null, (int)LogicOperators.OR); }
        public static RPNLogicEntry False { get => new(null, (int)LogicOperators.NONE); }
        public static RPNLogicEntry True { get => new(null, (int)LogicOperators.ANY); }
    }
}

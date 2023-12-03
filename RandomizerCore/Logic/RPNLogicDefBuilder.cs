using RandomizerCore.StringLogic;
using OperatorToken = RandomizerCore.StringLogic.OperatorToken;
using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Logic
{
    internal readonly struct RPNLogicDefBuilder
    {
        private readonly string source;
        private readonly List<int> logic;
        private readonly AllowedVariableTypes variableTypes;
        private readonly LogicManager lm;

        [Flags]
        public enum AllowedVariableTypes
        {
            None = 0,
            LogicInt = 1,
            StateAccessVariable = 2,
            StateModifier = 4,
            StateProvider = 8,
            Any = ~0,
        }


        public RPNLogicDefBuilder(LogicManager lm, string source, AllowedVariableTypes variableTypes)
        {
            this.lm = lm;
            this.source = source;
            this.variableTypes = variableTypes;
            this.logic = new();
        }

        public void Process(IEnumerable<LogicToken> tokens)
        {
            foreach (LogicToken lt in tokens) ApplyToken(lt);
        }

        public void MoveComparisonOperatorsAfterArgs() // some rpn logic formats are read backwards
        {
            for (int i = 0; i < logic.Count; i++)
            {
                switch (logic[i])
                {
                    case (int)LogicOperators.GT:
                    case (int)LogicOperators.LT:
                    case (int)LogicOperators.EQ:
                        {
                            int op = logic[i];
                            logic[i] = logic[i + 1];
                            logic[i + 1] = logic[i + 2];
                            logic[i + 2] = op;
                            i += 2;
                        }
                        break;
                }
            }
        }

        public int[] GetRawLogic() => logic.ToArray();

        private void ApplyTermOrVariable(string name)
        {
            if (lm.GetTerm(name) is Term t)
            {
                logic.Add(t.Id);
            }
            else if (lm.GetVariableID(name) is int variableID)
            {
                switch (lm.GetVariable(variableID))
                {
                    case LogicInt when variableTypes.HasFlag(AllowedVariableTypes.LogicInt):
                    case StateAccessVariable when variableTypes.HasFlag(AllowedVariableTypes.StateAccessVariable):
                    case StateModifier when variableTypes.HasFlag(AllowedVariableTypes.StateModifier):
                    case StateProvider when variableTypes.HasFlag(AllowedVariableTypes.StateProvider):
                        logic.Add(variableID);
                        break;
                    case LogicVariable lv:
                        throw new ArgumentException($"Variable {name} of type {lv.GetType()} is not valid for RPN logic with allowed variables {variableTypes}.");
                }


                logic.Add(variableID);
            }
            else throw new ArgumentException($"Unknown string {name} found as term.");
        }

        private void ApplyToken(LogicToken lt)
        {
            switch (lt)
            {
                case OperatorToken ot:
                    ApplyOperatorToken(ot);
                    break;
                case SimpleToken st:
                    ApplySimpleToken(st);
                    break;
                case ComparisonToken ct:
                    ApplyComparisonToken(ct);
                    break;
                case ConstToken bt:
                    ApplyConstToken(bt);
                    break;
                case MacroToken mt:
                    ApplyMacroToken(mt);
                    break;
                case ReferenceToken rt:
                    ApplyReferenceToken(rt);
                    break;
                case CoalescingToken qt:
                    ApplyCoalescingToken(qt);
                    break;
                default:
                    throw new ArgumentException($"Found unrecognized token in logic: {lt}");
            }
        }

        private void ApplyOperatorToken(OperatorToken ot)
        {
            logic.Add(ot.OperatorType switch
            {
                OperatorType.AND => (int)LogicOperators.AND,
                OperatorType.OR => (int)LogicOperators.OR,
                _ => throw new NotImplementedException()
            });
        }

        private void ApplySimpleToken(SimpleToken st)
        {
            ApplyTermOrVariable(st.Name);
        }

        private void ApplyComparisonToken(ComparisonToken ct)
        {
            logic.Add(ct.ComparisonType switch
            {
                ComparisonType.EQ => (int)LogicOperators.EQ,
                ComparisonType.LT => (int)LogicOperators.LT,
                ComparisonType.GT => (int)LogicOperators.GT,
                _ => throw new NotImplementedException(),
            });
            ApplyTermOrVariable(ct.Left);
            ApplyTermOrVariable(ct.Right);
        }

        private void ApplyConstToken(ConstToken bt)
        {
            logic.Add(bt.Value ? (int)LogicOperators.ANY : (int)LogicOperators.NONE);
        }

        private void ApplyMacroToken(MacroToken mt)
        {
            foreach (LogicToken tt in mt.Value) ApplyToken(tt);
        }

        private void ApplyReferenceToken(ReferenceToken rt)
        {
            LogicDef def = lm.ResolveLogicDefReference(source, rt.Target);
            foreach (LogicToken tt in def.ToTokenSequence()) ApplyToken(tt);
        }

        private void ApplyCoalescingToken(CoalescingToken qt)
        {
            if (IsValidToken(qt.Left)) ApplyToken(qt.Left);
            else ApplyToken(qt.Right);
        }

        private bool IsValidToken(LogicToken lt)
        {
            return lt switch
            {
                OperatorToken => true,
                SimpleToken st => lm.IsTermOrVariable(st.Name),
                ComparisonToken ct => lm.IsTermOrVariable(ct.Left) && lm.IsTermOrVariable(ct.Right),
                ConstToken => true,
                MacroToken mt => mt.Source?.GetMacro(mt.Name) is not null,
                CoalescingToken qt => IsValidToken(qt.Left) || IsValidToken(qt.Right),
                _ => false,
            };
        }
    }
}

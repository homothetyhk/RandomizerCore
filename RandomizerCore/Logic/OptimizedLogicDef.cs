using Newtonsoft.Json;
using RandomizerCore.StringLogic;

namespace RandomizerCore.Logic
{
    public class OptimizedLogicDef : ILogicDef
    {
        public OptimizedLogicDef(string Name, int[] logic, LogicManager lm)
        {
            if (logic == null || logic.Length == 0) throw new ArgumentException($"Invalid logic array passed to OptimizedLogicDef for {Name}");

            this.Name = Name;
            this.lm = lm;
            this.logic = logic;
        }

        public OptimizedLogicDef(OptimizedLogicDef def)
        {
            this.Name = def.Name;
            this.lm = def.lm;
            this.logic = def.logic;
        }

        public string Name { get; }

        private readonly int[] logic;
        private readonly LogicManager lm;

        // recycling the stack saves approx 0.03s in 129000 calls of CanGet (i.e. standard preset)
        // we don't ever clear the stack -- this causes no issues if all logic has correct syntax.
        static readonly Stack<bool> stack = new();

        public bool CanGet(ProgressionManager pm)
        {
            if (logic == null || logic.Length == 0) throw new InvalidOperationException($"Invalid logic array found in OptimizedLogicDef for {Name}");

            for (int i = 0; i < logic.Length; i++)
            {
                switch (logic[i])
                {
                    case (int)LogicOperators.AND:
                        stack.Push(stack.Pop() & stack.Pop());
                        continue;
                    case (int)LogicOperators.OR:
                        stack.Push(stack.Pop() | stack.Pop());
                        continue;
                    case (int)LogicOperators.NONE:
                        stack.Push(false);
                        continue;
                    case (int)LogicOperators.ANY:
                        stack.Push(true);
                        continue;
                    case (int)LogicOperators.GT:
                        {
                            int left = logic[++i];
                            left = left >= 0 ? pm.Get(left) : lm.EvaluateVariable(this, pm, left);
                            int right = logic[++i];
                            right = right >= 0 ? pm.Get(right) : lm.EvaluateVariable(this, pm, right);
                            stack.Push(left > right);
                        }
                        break;
                    case (int)LogicOperators.LT:
                        {
                            int left = logic[++i];
                            left = left >= 0 ? pm.Get(left) : lm.EvaluateVariable(this, pm, left);
                            int right = logic[++i];
                            right = right >= 0 ? pm.Get(right) : lm.EvaluateVariable(this, pm, right);
                            stack.Push(left < right);
                        }
                        break;
                    case (int)LogicOperators.EQ:
                        {
                            int left = logic[++i];
                            left = left >= 0 ? pm.Get(left) : lm.EvaluateVariable(this, pm, left);
                            int right = logic[++i];
                            right = right >= 0 ? pm.Get(right) : lm.EvaluateVariable(this, pm, right);
                            stack.Push(left == right);
                        }
                        break;
                    default:
                        stack.Push(logic[i] >= 0 ? pm.Has(logic[i]) : lm.EvaluateVariable(this, pm, logic[i]) > 0);
                        break;
                }
            }

            return stack.Pop();
        }

        /// <summary>
        /// Enumerates the terms of the LogicDef, excluding operators and combinators. May contain duplicates.
        /// </summary>
        public IEnumerable<Term> GetTerms()
        {
            for (int i = 0; i < logic.Length; i++)
            {
                switch (logic[i])
                {
                    case (int)LogicOperators.AND:
                    case (int)LogicOperators.OR:
                    case (int)LogicOperators.NONE:
                    case (int)LogicOperators.ANY:
                        continue;
                    case (int)LogicOperators.GT:
                    case (int)LogicOperators.LT:
                    case (int)LogicOperators.EQ:
                        {
                            int left = logic[++i];
                            if (left >= 0) yield return lm.GetTerm(left);
                            else foreach (Term t in lm.GetVariable(left).GetTerms()) yield return t;
                            int right = logic[++i];
                            if (right >= 0) yield return lm.GetTerm(right);
                            else foreach (Term t in lm.GetVariable(right).GetTerms()) yield return t;
                        }
                        break;
                    default:
                        {
                            if (logic[i] >= 0) yield return lm.GetTerm(logic[i]);
                            else foreach (Term t in lm.GetVariable(logic[i]).GetTerms()) yield return t;
                        }
                        break;
                }
            }
        }

        public LogicClauseBuilder ToLogicClauseBuilder()
        {
            LogicClauseBuilder lcb = new();
            for (int i = 0; i < logic.Length; i++)
            {
                switch (logic[i])
                {
                    case (int)LogicOperators.NONE:
                        lcb.Append(ConstToken.False);
                        break;
                    case (int)LogicOperators.ANY:
                        lcb.Append(ConstToken.True);
                        break;
                    case (int)LogicOperators.OR:
                        lcb.Append(OperatorToken.OR);
                        break;
                    case (int)LogicOperators.AND:
                        lcb.Append(OperatorToken.AND);
                        break;
                    case (int)LogicOperators.EQ:
                        {
                            GetComparisonStrings(ref i, out string left, out string right);
                            lcb.Append(lm.LP.GetComparisonToken(ComparisonType.EQ, left, right));
                        }
                        break;
                    case (int)LogicOperators.LT:
                        {
                            GetComparisonStrings(ref i, out string left, out string right);
                            lcb.Append(lm.LP.GetComparisonToken(ComparisonType.LT, left, right));
                        }
                        break;
                    case (int)LogicOperators.GT:
                        {
                            GetComparisonStrings(ref i, out string left, out string right);
                            lcb.Append(lm.LP.GetComparisonToken(ComparisonType.GT, left, right));
                        }
                        break;
                    default:
                        lcb.Append(lm.LP.GetTermToken(logic[i] >= 0 ? lm.GetTerm(logic[i]).Name : lm.GetVariable(logic[i]).Name));
                        break;
                }
            }
            return lcb;
        }

        public LogicClause ToLogicClause()
        {
            return new(ToLogicClauseBuilder());
        }

        public string ToInfix()
        {
            return ToLogicClauseBuilder().ToInfix();
        }

        private void GetComparisonStrings(ref int i, out string left, out string right)
        {
            i++;
            left = logic[i] >= 0 ? lm.GetTerm(logic[i]).Name : lm.GetVariable(logic[i]).Name;
            i++;
            right = logic[i] >= 0 ? lm.GetTerm(logic[i]).Name : lm.GetVariable(logic[i]).Name;
        }

        // cursed hacks below to make polymorphic deserialization work
        [JsonConstructor]
        private OptimizedLogicDef(string Name, string Logic) : this(Json.LogicDefConverter.Instance.LM.FromString(new(Name, Logic)))
        {
        }
        [JsonProperty("Logic")] private string Infix { get => ToInfix(); }
    }
}

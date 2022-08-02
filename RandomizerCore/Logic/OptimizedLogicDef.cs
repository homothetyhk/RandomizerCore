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

        private readonly Stack<bool> stack = new();

        public bool CanGet(ProgressionManager pm)
        {
            try
            {
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
            catch (Exception)
            {
                LogicError();
                throw;
            }
        }

        private void LogicError()
        {
            Log($"Error evaluating OptimizedLogicDef {Name}");
            try
            {
                string infix = Infix;
                Log($"Infix logic for {Name}: {infix}");
            }
            catch
            {
                Log($"Unable to get infix logic for {Name}");
                try
                {
                    Log($"Token list: {string.Join(" ", ToTokenSequence().Select(t => t is TermToken tt ? tt.Write() : t is OperatorToken ot ? ot.Symbol : ""))}");
                }
                catch (Exception e)
                {
                    Log($"Unable to get token list for {Name}:\n{e}");
                }
            }
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
        
        public IEnumerable<LogicToken> ToTokenSequence()
        {
            for (int i = 0; i < logic.Length; i++)
            {
                switch (logic[i])
                {
                    case (int)LogicOperators.NONE:
                        yield return ConstToken.False;
                        break;
                    case (int)LogicOperators.ANY:
                        yield return ConstToken.True;
                        break;
                    case (int)LogicOperators.OR:
                        yield return OperatorToken.OR;
                        break;
                    case (int)LogicOperators.AND:
                        yield return OperatorToken.AND;
                        break;
                    case (int)LogicOperators.EQ:
                        {
                            GetComparisonStrings(ref i, out string left, out string right);
                            yield return lm.LP.GetComparisonToken(ComparisonType.EQ, left, right);
                        }
                        break;
                    case (int)LogicOperators.LT:
                        {
                            GetComparisonStrings(ref i, out string left, out string right);
                            yield return lm.LP.GetComparisonToken(ComparisonType.LT, left, right);
                        }
                        break;
                    case (int)LogicOperators.GT:
                        {
                            GetComparisonStrings(ref i, out string left, out string right);
                            yield return lm.LP.GetComparisonToken(ComparisonType.GT, left, right);
                        }
                        break;
                    default:
                        yield return lm.LP.GetTermToken(logic[i] >= 0 ? lm.GetTerm(logic[i]).Name : lm.GetVariable(logic[i]).Name);
                        break;
                }
            }
        }

        public LogicClauseBuilder ToLogicClauseBuilder()
        {
            return new(ToTokenSequence());
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

        internal static void Concat(List<int> ts, OptimizedLogicDef o)
        {
            ts.AddRange(o.logic);
        }

        // cursed hacks below to make polymorphic deserialization work
        [JsonConstructor]
        private OptimizedLogicDef(string Name, string Logic) : this(Json.LogicDefConverter.Instance.LM.FromString(new(Name, Logic)))
        {
        }
        [JsonProperty("Logic")] private string Infix { get => ToInfix(); }
    }
}

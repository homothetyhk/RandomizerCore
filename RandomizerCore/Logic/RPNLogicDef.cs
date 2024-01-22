using RandomizerCore.StringLogic;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// A <see cref="LogicDef"/> which stores its contents in reverse Polish notation.
    /// </summary>
    public class RPNLogicDef : LogicDef
    {
        public RPNLogicDef(string Name, int[] logic, LogicManager lm, string infixSource) : base(Name, infixSource)
        {
            if (logic == null || logic.Length == 0) throw new ArgumentException($"Invalid logic array passed to RPNLogicDef for {Name}");

            this.lm = lm;
            this.logic = logic;
        }

        public RPNLogicDef(RPNLogicDef def) : base(def.Name, def.InfixSource)
        {
            this.lm = def.lm;
            this.logic = def.logic;
        }

        private readonly int[] logic;
        private readonly LogicManager lm;

        private readonly Stack<bool> stack = new();

        public override bool CanGet(ProgressionManager pm)
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
                                left = left >= 0 ? pm.Get(left) : EvaluateVariable(left, pm);
                                int right = logic[++i];
                                right = right >= 0 ? pm.Get(right) : EvaluateVariable(right, pm);
                                stack.Push(left > right);
                            }
                            break;
                        case (int)LogicOperators.LT:
                            {
                                int left = logic[++i];
                                left = left >= 0 ? pm.Get(left) : EvaluateVariable(left, pm);
                                int right = logic[++i];
                                right = right >= 0 ? pm.Get(right) : EvaluateVariable(right, pm);
                                stack.Push(left < right);
                            }
                            break;
                        case (int)LogicOperators.EQ:
                            {
                                int left = logic[++i];
                                left = left >= 0 ? pm.Get(left) : EvaluateVariable(left, pm);
                                int right = logic[++i];
                                right = right >= 0 ? pm.Get(right) : EvaluateVariable(right, pm);
                                stack.Push(left == right);
                            }
                            break;
                        default:
                            stack.Push(logic[i] >= 0 ? pm.Has(logic[i]) : EvaluateVariable(logic[i], pm) > 0);
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
                string infix = ToInfix();
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

        private int EvaluateVariable(int id, ProgressionManager pm)
        {
            return lm.GetVariable(id) switch
            {
                LogicInt li => li.GetValue(this, pm),
                _ => 0,
            };
        }

        /// <summary>
        /// Enumerates the terms of the LogicDef, excluding operators and combinators. May contain duplicates.
        /// </summary>
        public override IEnumerable<Term> GetTerms()
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
        
        public override IEnumerable<LogicToken> ToTokenSequence()
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

        private void GetComparisonStrings(ref int i, out string left, out string right)
        {
            i++;
            left = logic[i] >= 0 ? lm.GetTerm(logic[i]).Name : lm.GetVariable(logic[i]).Name;
            i++;
            right = logic[i] >= 0 ? lm.GetTerm(logic[i]).Name : lm.GetVariable(logic[i]).Name;
        }
    }
}

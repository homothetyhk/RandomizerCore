using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using static RandomizerCore.LogHelper;

namespace RandomizerCore.Logic
{
    public class LogicDef : ILogicDef
    {
        public LogicDef(string Name, string RawLogic, int[] logic, LogicManager lm)
        {
            this.Name = Name;
            this.RawLogic = RawLogic;
            this.lm = lm;
            this.logic = logic;
        }

        public string Name { get; }
        public string RawLogic { get; }
        private readonly int[] logic;
        private readonly LogicManager lm;

        // recycling the stack saves approx 0.03s in 129000 calls of CanGet (i.e. standard preset)
        // we don't ever clear the stack -- this causes no issues if all logic has correct syntax.
        static readonly Stack<bool> stack = new();

        public bool CanGet(ProgressionManager pm)
        {
            if (logic == null || logic.Length == 0) return true;

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
    }
}

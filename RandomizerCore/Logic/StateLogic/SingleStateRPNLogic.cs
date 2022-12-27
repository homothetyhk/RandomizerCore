using RandomizerCore.StringLogic;

namespace RandomizerCore.Logic.StateLogic
{
    internal class SingleStateRPNLogic : SingleStateLogic
    {
        public SingleStateRPNLogic(string name, int[] logic, LogicManager lm, string infixSource) : base(name, infixSource)
        {
            if (logic == null || logic.Length == 0) throw new ArgumentException($"Invalid logic array passed to RPNLogicDef for {name}");

            this.lm = lm;
            this.logic = logic;
        }

        private readonly int[] logic;
        private readonly LogicManager lm;

        public override bool CanGet<T>(ProgressionManager pm, T state)
        {
            try
            {
                int i = logic.Length - 1;
                return Eval(pm, state, ref i);
            }
            catch (Exception e)
            {
                throw ThrowHelper(e);
            }
        }

        private bool Eval<T>(ProgressionManager pm, T state, ref int i) where T : IState
        {
            int id = logic[i--];
            switch (id)
            {
                case (int)LogicOperators.AND:
                    return Eval(pm, state, ref i) & Eval(pm, state, ref i);
                case (int)LogicOperators.OR:
                    return Eval(pm, state, ref i) | Eval(pm, state, ref i);
                case (int)LogicOperators.NONE:
                    return false;
                case (int)LogicOperators.ANY:
                    return true;
                case (int)LogicOperators.GT:
                    {
                        int right = ParseNextInt(pm, state, logic[i--]);
                        int left = ParseNextInt(pm, state, logic[i--]);
                        return left > right;
                    }
                case (int)LogicOperators.LT:
                    {
                        int right = ParseNextInt(pm, state, logic[i--]);
                        int left = ParseNextInt(pm, state, logic[i--]);
                        return left < right;
                    }
                case (int)LogicOperators.EQ:
                    {
                        int right = ParseNextInt(pm, state, logic[i--]);
                        int left = ParseNextInt(pm, state, logic[i--]);
                        return left == right;
                    }
                default:
                    return ParseNextInt(pm, state, id) > 0;
            }
        }

        private int ParseNextInt<T>(ProgressionManager pm, T state, int id) where T : IState => id switch
        {
            < -99 => GetVariable(pm, state, id),
            >= 0 => pm.Get(id),
            _ => throw new NotImplementedException(),
        };

        private int GetVariable<T>(ProgressionManager pm, T state, int id) where T : IState => lm.GetVariable(id) switch
        {
            LogicInt li => li.GetValue(this, pm),
            StateAccessVariable sav => sav.GetValue(this, pm, state),
            _ => throw new NotImplementedException(),
        };



        private Exception ThrowHelper()
        {
            return new InvalidOperationException($"Error evaluating {GetType().Name} {Name} with source {InfixSource}");
        }

        private Exception ThrowHelper(Exception e)
        {
            return new InvalidOperationException($"Error evaluating {GetType().Name} {Name} with source {InfixSource}", e);
        }

        /// <summary>
        /// Enumerates the terms of the LogicDef, excluding operators and combinators. May contain duplicates.
        /// </summary>
        public override IEnumerable<Term> GetTerms()
        {
            for (int i = logic.Length - 1; i >= 0; i--)
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
                            int right = logic[--i];
                            if (right >= 0) yield return lm.GetTerm(right);
                            else foreach (Term t in lm.GetVariable(right).GetTerms()) yield return t;
                            int left = logic[--i];
                            if (left >= 0) yield return lm.GetTerm(left);
                            else foreach (Term t in lm.GetVariable(left).GetTerms()) yield return t;
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

        public IEnumerable<StateField> GetStateFields()
        {
            for (int i = logic.Length - 1; i >= 0; i--)
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
                            int right = logic[--i];
                            if (right < 0 && lm.GetVariable(right) is StateAccessVariable savR)
                            {
                                foreach (StateField sf in savR.GetStateFields()) yield return sf;
                            }
                            int left = logic[--i];
                            if (left < 0 && lm.GetVariable(left) is StateAccessVariable savL)
                            {
                                foreach (StateField sf in savL.GetStateFields()) yield return sf;
                            }
                        }
                        break;
                    default:
                        {
                            if (logic[i] < 0 && lm.GetVariable(logic[i]) is StateAccessVariable sav)
                            {
                                foreach (StateField sf in sav.GetStateFields()) yield return sf;
                            }
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
                    default:
                        if (i + 2 < logic.Length)
                        {
                            switch (logic[i + 2])
                            {
                                case (int)LogicOperators.EQ:
                                    {
                                        GetComparisonStrings(ref i, out string left, out string right);
                                        yield return lm.LP.GetComparisonToken(ComparisonType.EQ, left, right);
                                    }
                                    continue;
                                case (int)LogicOperators.LT:
                                    {
                                        GetComparisonStrings(ref i, out string left, out string right);
                                        yield return lm.LP.GetComparisonToken(ComparisonType.LT, left, right);
                                    }
                                    continue;
                                case (int)LogicOperators.GT:
                                    {
                                        GetComparisonStrings(ref i, out string left, out string right);
                                        yield return lm.LP.GetComparisonToken(ComparisonType.GT, left, right);
                                    }
                                    continue;
                            }
                        }
                        yield return lm.LP.GetTermToken(logic[i] >= 0 ? lm.GetTerm(logic[i]).Name : lm.GetVariable(logic[i]).Name);
                        break;
                }
            }
        }

        private void GetComparisonStrings(ref int i, out string left, out string right)
        {
            left = logic[i] >= 0 ? lm.GetTerm(logic[i]).Name : lm.GetVariable(logic[i]).Name;
            i++;
            right = logic[i] >= 0 ? lm.GetTerm(logic[i]).Name : lm.GetVariable(logic[i]).Name;
            i++;
        }
    }
}

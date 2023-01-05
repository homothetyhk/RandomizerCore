using Newtonsoft.Json;
using RandomizerCore.Json;
using RandomizerCore.Logic.StateLogic;
using RandomizerCore.StringLogic;
using System.ComponentModel;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// A <see cref="LogicDef"/> which stores its contents in disjunctive normal form.
    /// <br/>Preferred for operations which need to inspect the successful subexpression after evaluation, such as most state operations.
    /// </summary>
    public class DNFLogicDef : StateLogicDef
    {
        private readonly Clause[] clauses;
        private readonly LogicManager lm;

        internal struct Clause
        {
            public int[] logic;
            public int[] stateLogic;
            public int stateProvider;
        }

        internal DNFLogicDef(Clause[] clauses, LogicManager lm, string name, string infixSource) : base(name, infixSource)
        {
            this.clauses = clauses;
            this.lm = lm;
        }

        public DNFLogicDef(DNFLogicDef other) : base(other.Name, other.InfixSource)
        {
            this.clauses = other.clauses;
            this.lm = other.lm;
        }

        public override bool CanGet(ProgressionManager pm)
        {
            for (int j = 0; j < clauses.Length; j++)
            {
                if (EvaluateClause(j, pm)) return true;
            }
            return false;
        }

        private bool EvaluateClause(int j, ProgressionManager pm)
        {
            StateUnion? state = GetClauseInputState(j, pm);
            return EvaluateClauseLogic(j, pm, state) && EvaluateStateDiscard(j, pm, state);
        }

        public override List<State> EvaluateState(ProgressionManager pm)
        {
            List<State> states = new();
            for (int j = 0; j < clauses.Length; j++)
            {
                StateUnion? input = GetClauseInputState(j, pm);
                if (input is null || !EvaluateClauseLogic(j, pm, input)) continue;
                int[] clause = clauses[j].stateLogic;
                if (clause.Length == 0)
                {
                    for (int i = 0; i < input.Count; i++) states.Add(input[i]);
                }
                else
                {
                    for (int k = 0; k < input.Count; k++)
                    {
                        EvaluateStateChangeRec(clause, 0, pm, states, new(input[k]));
                    }
                    EmptyEvaluateStateChangeRec(clause, 0, pm, states);
                }
            }
            return states;
        }

        private bool EvaluateClauseLogic(int j, ProgressionManager pm, StateUnion? state)
        {
            int[] clause = clauses[j].logic;

            for (int i = 0; i < clause.Length; i++)
            {
                int id = clause[i];
                switch (id)
                {
                    case >= 0:
                        if (pm.Has(id)) continue;
                        break;
                    case (int)LogicOperators.ANY:
                        continue;
                    case (int)LogicOperators.NONE:
                        break;
                    case (int)LogicOperators.EQ or (int)LogicOperators.LT or (int)LogicOperators.GT:
                        LogicOperators op = (LogicOperators)id;
                        id = clause[++i];
                        int left = id >= 0 ? pm.Get(id) : EvaluateVariable(id, pm, state);
                        id = clause[++i];
                        int right = id >= 0 ? pm.Get(id) : EvaluateVariable(id, pm, state);
                        if (op switch
                        {
                            LogicOperators.EQ => left == right,
                            LogicOperators.LT => left < right,
                            _ => left > right,
                        }) continue;
                        break;
                    default:
                        if (EvaluateVariable(id, pm, state) > 0) continue;
                        break;
                }
                return false;
            }
            return true;
        }

        private bool EvaluateStateDiscard(int j, ProgressionManager pm, StateUnion? state)
        {
            int[] clause = clauses[j].stateLogic;
            if (clause.Length == 0)
            {
                return true;
            }
            if (state is null)
            {
                return false;
            }
            for (int i = 0; i < state.Count; i++)
            {
                if (EvaluateStateDiscardRec(clause, 0, pm, new LazyStateBuilder(state[i]))) return true;
            }
            return EmptyEvaluateStateDiscardRec(clause, 0, pm);
        }

        private bool EvaluateStateDiscardRec(int[] clause, int i, ProgressionManager pm, LazyStateBuilder lsb)
        {
            if (i == clause.Length)
            {
                return true;
            }

            int id = clause[i];
            if (id < -99)
            {
                foreach (LazyStateBuilder lsb2 in ((StateModifier)lm.GetVariable(id)).ModifyState(this, pm, lsb))
                {
                    if (EvaluateStateDiscardRec(clause, i + 1, pm, lsb2)) return true;
                }
            }
            else
            {
                LogicOperators op = (LogicOperators)id;
                int left = EvaluateStateVariable(clause[i + 1], pm, lsb);
                int right = EvaluateStateVariable(clause[i + 1], pm, lsb);
                if (op switch
                {
                    LogicOperators.GT => left > right,
                    LogicOperators.LT => left < right,
                    LogicOperators.EQ => left == right,
                    _ => false
                })
                {
                    if (EvaluateStateDiscardRec(clause, i + 3, pm, lsb)) return true;
                }
            }
            
            return false;
        }

        private bool EmptyEvaluateStateDiscardRec(int[] clause, int i, ProgressionManager pm)
        {
            if (i == clause.Length)
            {
                return true;
            }

            int id = clause[i];
            if (id < -99 && ((StateModifier)lm.GetVariable(id)).ProvideState(this, pm) is IEnumerable<LazyStateBuilder> lsbs)
            {
                foreach (LazyStateBuilder lsb2 in lsbs)
                {
                    if (EvaluateStateDiscardRec(clause, i + 1, pm, lsb2)) return true;
                }
                if (EmptyEvaluateStateDiscardRec(clause, i + 1, pm)) return true;
            }

            return false;
        }

        private void EvaluateStateChangeRec(int[] clause, int i, ProgressionManager pm, List<State> states, LazyStateBuilder lsb)
        {
            if (i == clause.Length)
            {
                states.Add(lsb.GetState());
                return;
            }

            int id = clause[i];
            if (id < -99)
            {
                foreach (LazyStateBuilder lsb2 in  ((StateModifier)lm.GetVariable(id)).ModifyState(this, pm, lsb))
                {
                    EvaluateStateChangeRec(clause, i + 1, pm, states, lsb2);
                }
            }
            else
            {
                LogicOperators op = (LogicOperators)id;
                int left = EvaluateStateVariable(id + 1, pm, lsb);
                int right = EvaluateStateVariable(id + 2, pm, lsb);
                if (op switch
                {
                    LogicOperators.GT => left > right,
                    LogicOperators.LT => left < right,
                    LogicOperators.EQ => left == right,
                    _ => false
                })
                {
                    EvaluateStateChangeRec(clause, i + 3, pm, states, lsb);
                }
            }   
        }

        private void EmptyEvaluateStateChangeRec(int[] clause, int i, ProgressionManager pm, List<State> states)
        {
            if (i == clause.Length)
            {
                return;
            }

            int id = clause[i];
            if (((StateModifier)lm.GetVariable(id)).ProvideState(this, pm) is IEnumerable<LazyStateBuilder> lsbs)
            {
                foreach (LazyStateBuilder lsb2 in lsbs)
                {
                    EvaluateStateChangeRec(clause, i, pm, states, lsb2);
                }
            }
            EmptyEvaluateStateChangeRec(clause, i + 1, pm, states);
        }

        private int EvaluateVariable(int id, ProgressionManager pm, StateUnion? state)
        {
            return lm.GetVariable(id) switch
            {
                LogicInt li => li.GetValue(this, pm),
                _ => 0,
            };
        }

        private int EvaluateStateVariable(int id, ProgressionManager pm, LazyStateBuilder state)
        {
            if (id >= 0) return pm.Get(id);

            return lm.GetVariable(id) switch
            {
                LogicInt li => li.GetValue(this, pm),
                StateAccessVariable sav => sav.GetValue(this, pm, state),
                _ => 0,
            };
        }

        private StateUnion? GetClauseInputState(int j, ProgressionManager pm)
        {
            int inputID = clauses[j].stateProvider;
            switch (inputID)
            {
                case >= 0:
                    return pm.GetState(inputID);
                case <= LogicManager.intVariableOffset:
                    if (lm.GetVariable(inputID) is StateProvider spv) return spv.GetInputState(this, pm);
                    break;
            }
            return null;
        }

        public List<TermToken>? GetFirstSuccessfulConjunction(ProgressionManager pm)
        {
            for (int j = 0; j < clauses.Length; j++)
            {
                if (EvaluateClause(j, pm)) return ConvertClause(j);
            }
            return null;
        }

        public List<List<TermToken>> GetAllSuccessfulConjunctions(ProgressionManager pm)
        {
            List<List<TermToken>> successes = new();
            for (int j = 0; j < clauses.Length; j++)
            {
                if (EvaluateClause(j, pm)) successes.Add(ConvertClause(j));
            }
            return successes;
        }

        private TermToken ConvertToken(int[] clause, ref int i)
        {
            int id = clause[i];
            switch (id)
            {
                case >= 0:
                    return lm.LP.GetTermToken(lm.Terms[id].Name);
                case (int)LogicOperators.ANY:
                    return ConstToken.True;
                case (int)LogicOperators.NONE:
                    return ConstToken.False;
                case (int)LogicOperators.AND:
                    throw new NotSupportedException();
                case (int)LogicOperators.EQ or (int)LogicOperators.LT or (int)LogicOperators.GT:
                    LogicOperators op = (LogicOperators)id;
                    id = clause[++i];
                    string left = id >= 0 ? lm.GetTerm(id).Name : lm.GetVariable(id).Name;
                    id = clause[++i];
                    string right = id >= 0 ? lm.GetTerm(id).Name : lm.GetVariable(id).Name;
                    return op switch
                    {
                        LogicOperators.EQ => lm.LP.GetComparisonToken(ComparisonType.EQ, left, right),
                        LogicOperators.LT => lm.LP.GetComparisonToken(ComparisonType.LT, left, right),
                        _ => lm.LP.GetComparisonToken(ComparisonType.GT, left, right),
                    };
                default:
                    return lm.LP.GetTermToken(lm.GetVariable(id).Name);
            }
        }

        private TermToken ConvertToken(StateModifier sm)
        {
            return lm.LP.GetTermToken(sm.Name);
        }

        private List<TermToken> ConvertClause(int j)
        {
            List<TermToken> result = new();

            Clause c = clauses[j];
            for (int i = 0; i < c.logic.Length; i++)
            {
                result.Add(ConvertToken(c.logic, ref i));
            }
            for (int i = 0; i < c.stateLogic.Length; i++)
            {
                result.Add(ConvertToken(c.logic, ref i));
            }

            return result;
        }

        public override IEnumerable<LogicToken> ToTokenSequence()
        {
            return RPN.OperateOver(Enumerable.Range(0, clauses.Length).Select(j => RPN.OperateOver(ConvertClause(j), OperatorToken.AND)), OperatorToken.OR);
        }

        public override IEnumerable<Term> GetTerms()
        {
            for (int j = 0; j < clauses.Length; j++)
            {
                Clause c = clauses[j];
                int[] logic = c.logic;
                for (int i = 0; i < logic.Length; i++)
                {
                    switch (logic[i])
                    {
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
                logic = c.stateLogic;
                for (int i = 0; i < logic.Length; i++)
                {
                    switch (logic[i])
                    {
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

        // cursed hacks for deserialization into ILogicDef property type, where the converter doesn't trigger.
        [JsonConstructor]
        protected DNFLogicDef(string Name, string Logic) : this(ConverterFetchOrMake(Name, Logic))
        {
        }

        private static DNFLogicDef ConverterFetchOrMake(string name, string logic)
        {
            if (LogicDefConverter.Instance.LM.GetLogicDef(name) is DNFLogicDef other && other.InfixSource == logic) return other;
            return LogicDefConverter.Instance.LM.CreateDNFLogicDef(new(name, logic));
        }
    }

    [Obsolete]
    [EditorBrowsable(EditorBrowsableState.Never)]
    public class OptimizedLogicDef : DNFLogicDef
    {
        public OptimizedLogicDef(DNFLogicDef def) : base(def)
        {
        }

        [JsonConstructor]
        protected OptimizedLogicDef(string Name, string Logic) : base(Name, Logic)
        {
        }
    }
}

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
        private readonly int[][] logic;
        private readonly int[] clauseStateProviders;
        private readonly LogicManager lm;

        public DNFLogicDef(int[][] logic, int[] clauseStateProviders, LogicManager lm, string name, string infixSource) : base(name, infixSource)
        {
            this.logic = logic;
            this.clauseStateProviders = clauseStateProviders;
            this.lm = lm;
        }

        public DNFLogicDef(DNFLogicDef other) : base(other.Name, other.InfixSource)
        {
            this.logic = other.logic;
            this.clauseStateProviders = other.clauseStateProviders;
            this.lm = other.lm;
        }

        public override bool CanGet(ProgressionManager pm)
        {
            for (int j = 0; j < logic.Length; j++)
            {
                if (EvaluateClause(pm, j)) return true;
            }
            return false;
        }

        private bool EvaluateClause(ProgressionManager pm, int j)
        {
            int[] clause = logic[j];
            StateUnion? state = GetClauseInputState(pm, j);

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

        public override List<State> EvaluateState(ProgressionManager pm)
        {
            List<State> states = new();
            for (int j = 0; j < logic.Length; j++)
            {
                //Console.WriteLine($"{j}  " + DNF.ToInfix(ConvertClause(logic[j])));
            }


            for (int j = 0; j < logic.Length; j++)
            {
                if (!EvaluateClause(pm, j)) continue;
                StateUnion? input = GetClauseInputState(pm, j);
                if (input is null) continue;
                for (int k = 0; k < input.Count; k++)
                {
                    EvaluateStateChange(logic[j], 0, pm, states, new(input[k]));
                }
                //Console.WriteLine($"{j}  {pm.lm.StateManager.PrettyPrint(new StateUnion(states))}");
            }
            return states;
        }

        // Evaluates the effect of the clause from index i onwards on the lsb, and adds the result to the state list.
        // If it produces a zero state, reduces the list to that state and returns true to allow shortcircuiting.
        private void EvaluateStateChange(int[] clause, int i, ProgressionManager pm, List<State> states, LazyStateBuilder lsb)
        {
            for (; i < clause.Length; i++)
            {
                if (clause[i] < -99)
                {
                    switch (lm.GetVariable(clause[i]))
                    {
                        case StateModifyingVariable smv:
                            if (!smv.ModifyState(this, pm, ref lsb)) return;
                            break;
                        case StateSplittingVariable ssv:
                            if (ssv.ModifyState(this, pm, lsb) is IEnumerable<LazyStateBuilder> lsbs)
                            {
                                foreach (var lsb2 in lsbs)
                                {
                                    EvaluateStateChange(clause, i + 1, pm, states, lsb2);
                                }
                            }
                            return;
                    }
                }
            }
            states.Add(lsb.GetState());
        }

        private int EvaluateVariable(int id, ProgressionManager pm, StateUnion? state)
        {
            return lm.GetVariable(id) switch
            {
                LogicInt li => li.GetValue(this, pm),
                StateVariable sv => sv.GetValue(this, pm, state),
                _ => 0,
            };
        }

        private StateUnion? GetClauseInputState(ProgressionManager pm, int j)
        {
            int inputID = clauseStateProviders[j];
            switch (inputID)
            {
                case >= 0:
                    return pm.GetState(inputID);
                case < -99:
                    if (lm.GetVariable(inputID) is StateProviderVariable spv) return spv.GetInputState(this, pm);
                    break;
            }
            return null;
        }

        public List<TermToken>? GetFirstSuccessfulConjunction(ProgressionManager pm)
        {
            for (int j = 0; j < logic.Length; j++)
            {
                if (EvaluateClause(pm, j)) return ConvertClause(logic[j]);
            }
            return null;
        }

        public List<List<TermToken>> GetAllSuccessfulConjunctions(ProgressionManager pm)
        {
            List<List<TermToken>> successes = new();
            for (int j = 0; j < logic.Length; j++)
            {
                if (EvaluateClause(pm, j)) successes.Add(ConvertClause(logic[j]));
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

        private List<TermToken> ConvertClause(int[] clause)
        {
            List<TermToken> result = new();
            for (int i = 0; i < clause.Length; i++)
            {
                result.Add(ConvertToken(clause, ref i));
            }
            return result;
        }

        public override IEnumerable<LogicToken> ToTokenSequence()
        {
            return RPN.OperateOver(logic.Select(c => RPN.OperateOver(ConvertClause(c), OperatorToken.AND)), OperatorToken.OR);
        }

        public override IEnumerable<Term> GetTerms()
        {
            for (int j = 0; j < logic.Length; j++)
            {
                int[] clause = logic[j];
                for (int i = 0; i < clause.Length; i++)
                {
                    switch (clause[i])
                    {
                        case (int)LogicOperators.NONE:
                        case (int)LogicOperators.ANY:
                            continue;
                        case (int)LogicOperators.GT:
                        case (int)LogicOperators.LT:
                        case (int)LogicOperators.EQ:
                            {
                                int left = clause[++i];
                                if (left >= 0) yield return lm.GetTerm(left);
                                else foreach (Term t in lm.GetVariable(left).GetTerms()) yield return t;
                                int right = clause[++i];
                                if (right >= 0) yield return lm.GetTerm(right);
                                else foreach (Term t in lm.GetVariable(right).GetTerms()) yield return t;
                            }
                            break;
                        default:
                            {
                                if (clause[i] >= 0) yield return lm.GetTerm(clause[i]);
                                else foreach (Term t in lm.GetVariable(clause[i]).GetTerms()) yield return t;
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

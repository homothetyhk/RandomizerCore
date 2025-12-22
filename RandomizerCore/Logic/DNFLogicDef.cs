using RandomizerCore.Logic.StateLogic;
using RandomizerCore.StringLogic;
using RandomizerCore.StringParsing;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using OT = RandomizerCore.StringLogic.OperatorToken;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// A <see cref="LogicDef"/> which stores its contents in disjunctive normal form.
    /// <br/>Preferred for operations which need to inspect the successful subexpression after evaluation, such as most state operations.
    /// </summary>
    public class DNFLogicDef : StateLogicDef
    {
        private readonly StatePath[] paths;
        private readonly LogicManager lm;
        private Dictionary<int, List<StatePath>>? termPathLookup;

        internal DNFLogicDef(string name, string infix, Expression<LogicExpressionType> expr, LogicManager lm) : base(name, infix)
        {
            this.lm = lm;
            paths = lm._dNFConverter.CreateLogicData(name, expr, lm, this);
            Profiling.EmitMetric("DNFLogicDef.PathCount", paths.Length);
            Profiling.EmitMetric("DNFLogicDef.ResultingTermCount", GetTerms().Count());
        }

        protected DNFLogicDef(DNFLogicDef other) : base(other.Name, other.InfixSource)
        {
            this.paths = other.paths;
            this.lm = other.lm;
            this.termPathLookup = other.termPathLookup;
        }

        public override bool CanGet(ProgressionManager pm)
        {
            Stopwatch sw = Stopwatch.StartNew();
            for (int j = 0; j < paths.Length; j++)
            {
                if (paths[j].EvaluateToBool(pm))
                {
                    sw.Stop();
                    Profiling.EmitMetric("DNFLogicDef.CanGet.RuntimeUs", sw.Elapsed.TotalMilliseconds * 1000);
                    return true;
                }
            }
            sw.Stop();
            Profiling.EmitMetric("DNFLogicDef.CanGet.RuntimeUs", sw.Elapsed.TotalMilliseconds * 1000);
            return false;
        }

        public override bool EvaluateState(ProgressionManager pm, List<State> states)
        {
            Stopwatch sw = Stopwatch.StartNew();
            bool succeedsOnEmpty = false;
            for (int j = 0; j < paths.Length; j++)
            {
                succeedsOnEmpty |= paths[j].EvaluateStateChange(pm, states);
            }
            sw.Stop();
            Profiling.EmitMetric("DNFLogicDef.EvaluateState.RuntimeUs", sw.Elapsed.TotalMilliseconds * 1000);
            return succeedsOnEmpty;
        }

        public override bool EvaluateStateFrom(ProgressionManager pm, IStateProvider stateProvider, List<State> states)
        {
            Stopwatch sw = Stopwatch.StartNew();
            bool succeedOnEmpty = false;
            foreach (StatePath path in paths.Where(p => ReferenceEquals(p.stateProvider, stateProvider)))
            {
                succeedOnEmpty |= path.EvaluateStateChange(pm, states);
            }
            sw.Stop();
            Profiling.EmitMetric("DNFLogicDef.EvaluateStateFrom.RuntimeUs", sw.Elapsed.TotalMilliseconds * 1000);
            return succeedOnEmpty;
        }

        public override bool CheckForUpdatedState(ProgressionManager pm, StateUnion? current, List<State> newStates, int modifiedTerm, [MaybeNullWhen(false)] out StateUnion result)
        {
            Stopwatch sw = Stopwatch.StartNew();
            if (termPathLookup is null) CreateTermPathLookup();

            bool succeedOnEmpty = false;
            foreach (StatePath c in termPathLookup![modifiedTerm])
            {
                succeedOnEmpty |= c.EvaluateStateChange(pm, newStates);
            }
            if (current is null)
            {
                if (newStates.Count > 0)
                {
                    result = new(newStates);
                    sw.Stop();
                    Profiling.EmitMetric("DNFLogicDef.CheckForUpdatedState.RuntimeUs", sw.Elapsed.TotalMilliseconds * 1000);
                    return true;
                }
                else if (succeedOnEmpty)
                {
                    result = lm.StateManager.Empty;
                    sw.Stop();
                    Profiling.EmitMetric("DNFLogicDef.CheckForUpdatedState.RuntimeUs", sw.Elapsed.TotalMilliseconds * 1000);
                    return true;
                }
                else
                {
                    result = null;
                    sw.Stop();
                    Profiling.EmitMetric("DNFLogicDef.CheckForUpdatedState.RuntimeUs", sw.Elapsed.TotalMilliseconds * 1000);
                    return false;
                }
            }
            else
            {
                bool succeeded = StateUnion.TryUnion(current, newStates, out result);
                sw.Stop();
                Profiling.EmitMetric("DNFLogicDef.CheckForUpdatedState.RuntimeUs", sw.Elapsed.TotalMilliseconds * 1000);
                return succeeded;
            }
        }

        public override IEnumerable<IStateProvider> GetStateProviders()
        {
            return paths.Select(p => p.stateProvider).Distinct();
        }

        /// <summary>
        /// Keeps states from true StatePaths separated, along with their corresponding true conjunctions.
        /// API for "explaining" a state evaluation.
        /// </summary>
        public bool DetailedEvaluateStateFrom(ProgressionManager pm, IStateProvider stateProvider, bool firstConjunctionsOnly, List<StatePathResult> results)
        {
            Stopwatch sw = Stopwatch.StartNew();
            bool succeedOnEmpty = false;
            foreach (StatePath path in paths.Where(p => p.stateProvider.Name == stateProvider.Name))
            {
                succeedOnEmpty |= path.DetailedEvaluateStateChange(pm, firstConjunctionsOnly, results);
            }
            sw.Stop();
            Profiling.EmitMetric("DNFLogicDef.DetailedEvaluateStateFrom.RuntimeUs", sw.Elapsed.TotalMilliseconds * 1000);
            return succeedOnEmpty;
        }

        private void CreateTermPathLookup()
        {
            termPathLookup = new();
            HashSet<int> termHelper = new();
            foreach (StatePath sp in paths)
            {
                termHelper.Clear();
                foreach (Term t in sp.GetTerms())
                {
                    if (termHelper.Add(t))
                    {
                        if (!termPathLookup.TryGetValue(t, out List<StatePath> sps))
                        {
                            termPathLookup.Add(t, sps = new());
                        }
                        sps.Add(sp);
                    }
                }
            }
        }

        public override Expression<LogicExpressionType> ToExpression()
        {
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
            return builder.ApplyInfixOperatorLeftAssoc(
                paths.Select(sp => sp.ToExpression())
                     .DefaultIfEmpty(builder.NameAtom("NONE")),
                builder.Op(LogicOperatorProvider.OR));
        }

        public ReadOnlyConjunction? GetFirstSuccessfulConjunction(ProgressionManager pm)
        {
            for (int j = 0; j < paths.Length; j++)
            {
                if (paths[j].GetFirstSuccessfulConjunction(pm) is ReadOnlyConjunction c) return c;
            }
            return null;
        }

        public IEnumerable<ReadOnlyConjunction> GetAllSuccessfulConjunctions(ProgressionManager pm)
        {
            for (int j = 0; j < paths.Length; j++)
            {
                foreach (ReadOnlyConjunction c in paths[j].GetAllSuccessfulConjunctions(pm)) yield return c;
            }
        }

        public IEnumerable<ReadOnlyConjunction> GetAllConjunctions()
        {
            return paths.SelectMany(p => p.GetAllConjunctions());
        }

        [Obsolete]
        public IEnumerable<IEnumerable<TermToken>> ToTermTokenSequences()
        {
            if (paths.Length == 0) return [[ConstToken.False]];
            return paths.SelectMany(p => p.ToTermTokenSequences());
        }

        public override IEnumerable<Term> GetTerms()
        {
            return paths.SelectMany(c => c.GetTerms());
        }

        internal readonly struct Reqs
        {
            private readonly TermValue[] termReqs;
            private readonly LogicInt[] varReqs;

            public Reqs(TermValue[] termReqs, LogicInt[] varReqs)
            {
                this.termReqs = termReqs;
                this.varReqs = varReqs;
            }

            public bool Evaluate(object? sender, ProgressionManager pm)
            {
                foreach (TermValue tv in termReqs) if (!pm.Has(tv)) return false;
                foreach (LogicInt li in varReqs) if (li.GetValue(sender, pm) <= 0) return false;
                return true;
            }

            public IEnumerable<Term> GetTerms()
            {
                foreach (TermValue tv in termReqs) yield return tv.Term;
                foreach (LogicInt li in varReqs) foreach (Term t in li.GetTerms()) yield return t;
            }

            public Expression<LogicExpressionType> ToExpression()
            {
                LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
                return builder.ApplyInfixOperatorLeftAssoc(
                           termReqs.Select(tv => tv.Value == 1
                                                ? builder.NameAtom(tv.Term.Name)
                                                : builder.ApplyInfixOperator(
                                                    builder.NameAtom(tv.Term.Name),
                                                    builder.Op(LogicOperatorProvider.GT),
                                                    builder.NumberAtom(tv.Value)))
                           .Concat(varReqs.Select(li => li.ToExpression()))
                           .DefaultIfEmpty(builder.NameAtom("ANY")),
                           builder.Op(LogicOperatorProvider.AND));
            }

            [Obsolete]
            internal IEnumerable<TermToken> ToTermTokenSequence(LogicManager lm)
            {
                foreach (TermValue tv in termReqs)
                {
                    yield return (TermToken)tv.ToExpression().ToLogicToken();
                }
                foreach (LogicInt li in varReqs) yield return (TermToken)li.ToExpression().ToLogicToken();
            }

            internal void GetReadOnlyData(out ReadOnlyCollection<TermValue> termReqs, out ReadOnlyCollection<LogicInt> varReqs)
            {
                termReqs = new(this.termReqs);
                varReqs = new(this.varReqs);
            }
        }

        internal class StatePath
        {
            public readonly Reqs[] reqs;
            public readonly IStateProvider? stateProvider;
            public readonly StateModifier[] stateModifiers;
            private readonly DNFLogicDef parent;

            public StatePath(Reqs[] reqs, IStateProvider? stateProvider, StateModifier[] stateModifiers, DNFLogicDef parent)
            {
                this.reqs = reqs;
                this.stateProvider = stateProvider;
                this.stateModifiers = stateModifiers;
                this.parent = parent;
            }

            public bool EvaluateStateChange(ProgressionManager pm, List<State> result)
            {
                if (stateProvider is null && stateModifiers.Length == 0) return EvaluateToBool(pm); // stateless case

                StateUnion? input = stateProvider?.GetInputState(parent, pm);
                if (input is null || !EvaluateAnyReqs(pm, out _)) return false;
                return EvaluateStateModifiersChangeRec(pm, input, result);
            }

            public bool DetailedEvaluateStateChange(ProgressionManager pm, bool firstConjunctionOnly, List<StatePathResult> results)
            {
                if (stateProvider is null && stateModifiers.Length == 0) return EvaluateToBool(pm); // stateless case

                if (stateProvider?.GetInputState(parent, pm) is not StateUnion input) return false;

                List<State> states = new();
                if (!firstConjunctionOnly && EvaluateAllReqs(pm, out IEnumerable<Reqs> reqs)
                    && EvaluateStateModifiersChangeRec(pm, input, states))
                {
                    results.Add(new StatePathResult(states, [..reqs.Select(r => new ReadOnlyConjunction(this, r))]));
                    return true;
                }

                if (firstConjunctionOnly && EvaluateAnyReqs(pm, out Reqs req)
                    && EvaluateStateModifiersChangeRec(pm, input, states))
                {
                    results.Add(new StatePathResult(states, [new(this, req)]));
                    return true;
                }

                return false;
            }

            public bool EvaluateToBool(ProgressionManager pm)
            {
                StateUnion? input = stateProvider?.GetInputState(parent, pm);
                if (input is null && stateProvider is not null || !EvaluateAnyReqs(pm, out _)) return false;
                return EvaluateStateModifiersDiscardRec(pm, input);
            }

            private bool EvaluateAnyReqs(ProgressionManager pm, out Reqs result)
            {
                foreach (Reqs r in reqs)
                {
                    if (r.Evaluate(parent, pm))
                    {
                        result = r;
                        return true;
                    }
                }
                result = default;
                return false;
            }

            private bool EvaluateAllReqs(ProgressionManager pm, out IEnumerable<Reqs> result)
            {
                result = reqs.Where(r => r.Evaluate(parent, pm));
                return result.Any();
            }

            private bool EvaluateStateModifiersDiscardRec(ProgressionManager pm, StateUnion input)
            {
                if (stateModifiers.Length == 0)
                {
                    return true;
                }

                for (int k = 0; k < input.Count; k++)
                {
                    if (EvaluateStateDiscardRec(0, pm, new LazyStateBuilder(input[k]))) return true;
                }
                
                return EmptyEvaluateStateDiscardRec(0, pm);
            }

            private bool EvaluateStateDiscardRec(int i, ProgressionManager pm, LazyStateBuilder lsb)
            {
                if (i == stateModifiers.Length)
                {
                    return true;
                }

                foreach (LazyStateBuilder lsb2 in stateModifiers[i].ModifyState(parent, pm, lsb))
                {
                    if (EvaluateStateDiscardRec(i + 1, pm, lsb2)) return true;
                }

                return false;
            }

            private bool EmptyEvaluateStateDiscardRec(int i, ProgressionManager pm)
            {
                if (i == stateModifiers.Length)
                {
                    return true;
                }

                if (stateModifiers[i].ProvideState(parent, pm) is IEnumerable<LazyStateBuilder> lsbs)
                {
                    foreach (LazyStateBuilder lsb2 in lsbs)
                    {
                        if (EvaluateStateDiscardRec(i + 1, pm, lsb2)) return true;
                    }
                    if (EmptyEvaluateStateDiscardRec(i + 1, pm)) return true;
                }

                return false;
            }

            private bool EvaluateStateModifiersChangeRec(ProgressionManager pm, StateUnion? input, List<State> result)
            {
                if (stateModifiers.Length == 0)
                {
                    for (int k = 0; k < input.Count; k++) result.Add(input[k]);
                    return true;
                }
                else
                {
                    for (int k = 0; k < input.Count; k++)
                    {
                        EvaluateStateChangeRec(0, pm, result, new(input[k]));
                    }
                    return EmptyEvaluateStateChangeRec(0, pm, result);
                }
            }

            private void EvaluateStateChangeRec(int i, ProgressionManager pm, List<State> states, LazyStateBuilder lsb)
            {
                if (i == stateModifiers.Length)
                {
                    states.Add(lsb.GetState());
                    return;
                }

                foreach (LazyStateBuilder lsb2 in stateModifiers[i].ModifyState(parent, pm, lsb))
                {
                    EvaluateStateChangeRec(i + 1, pm, states, lsb2);
                }
            }

            private bool EmptyEvaluateStateChangeRec(int i, ProgressionManager pm, List<State> states)
            {
                if (i == stateModifiers.Length)
                {
                    return true;
                }

                if (stateModifiers[i].ProvideState(parent, pm) is IEnumerable<LazyStateBuilder> lsbs)
                {
                    foreach (LazyStateBuilder lsb2 in lsbs)
                    {
                        EvaluateStateChangeRec(i, pm, states, lsb2);
                    }
                    return EmptyEvaluateStateChangeRec(i + 1, pm, states);
                }
                return false;
            }

            public IEnumerable<Term> GetTerms()
            {
                if (stateProvider is not null) foreach (Term t in stateProvider.GetTerms()) yield return t;
                foreach (Reqs r in reqs) foreach (Term t in r.GetTerms()) yield return t;
                foreach (StateModifier sm in stateModifiers) foreach (Term t in sm.GetTerms()) yield return t;
            }

            public Expression<LogicExpressionType> ToExpression()
            {
                LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
                return builder.ApplyInfixOperatorLeftAssoc(ToConjunctionArgs().DefaultIfEmpty(builder.NameAtom("NONE")), builder.Op(LogicOperatorProvider.AND));
            }

            // enumerates the state provider, the nonstate requirements as a single expression, and the state modifiers (as individual expressions)
            // intended so that distributing AND over the resulting sequence gives an expression for the path.
            private IEnumerable<Expression<LogicExpressionType>> ToConjunctionArgs()
            {
                if (stateProvider is not null) yield return stateProvider.ToExpression();
                yield return LogicExpressionUtil.Builder.ApplyInfixOperatorLeftAssoc(
                    reqs.Select(r => r.ToExpression()),
                    LogicExpressionUtil.Builder.Op(LogicOperatorProvider.OR));
                foreach (StateModifier sm in stateModifiers) yield return sm.ToExpression();
            }

            [Obsolete]
            public IEnumerable<IEnumerable<TermToken>> ToTermTokenSequences()
            {
                LogicManager lm = parent.lm;
                List<TermToken> suffix = [.. stateModifiers.Select(sm => (TermToken)sm.ToExpression().ToLogicToken())];
                return reqs.Select(r =>
                {
                    IEnumerable<TermToken> result = r.ToTermTokenSequence(lm).Concat(suffix);
                    if (stateProvider is not null) result = result.Prepend((TermToken)stateProvider.ToExpression().ToLogicToken());
                    return result.DefaultIfEmpty(ConstToken.True);
                });
            }

            public ReadOnlyConjunction? GetFirstSuccessfulConjunction(ProgressionManager pm)
            {
                if (stateProvider?.GetInputState(parent, pm) is StateUnion input
                    && EvaluateAnyReqs(pm, out Reqs result)
                    && EvaluateStateModifiersDiscardRec(pm, input))
                {
                    return new(this, result);
                }
                return null;
            }

            public IEnumerable<ReadOnlyConjunction> GetAllSuccessfulConjunctions(ProgressionManager pm)
            {
                if (stateProvider?.GetInputState(parent, pm) is StateUnion input
                    && EvaluateAllReqs(pm, out IEnumerable<Reqs> result)
                    && EvaluateStateModifiersDiscardRec(pm, input))
                {
                    return result.Select(r => new ReadOnlyConjunction(this, r));
                }
                return [];
            }

            public IEnumerable<ReadOnlyConjunction> GetAllConjunctions() => reqs.Select(r => new ReadOnlyConjunction(this, r));
        }

        public readonly struct StatePathResult
        {
            public readonly List<State> states;
            public readonly List<ReadOnlyConjunction> conjunctions;

            internal StatePathResult(List<State> states, List<ReadOnlyConjunction> conjunctions)
            {
                this.states = states;
                this.conjunctions = conjunctions;
            }
        }

        /// <summary>
        /// A struct carrying the data of a single branch of the DNF.
        /// </summary>
        public readonly struct ReadOnlyConjunction
        {
            public readonly IStateProvider? StateProvider;
            public readonly ReadOnlyCollection<TermValue> TermReqs;
            public readonly ReadOnlyCollection<LogicInt> VarReqs;
            public readonly ReadOnlyCollection<StateModifier> StateModifiers;

            public ReadOnlyConjunction(
                IStateProvider? stateProvider,
                ReadOnlyCollection<TermValue> termReqs,
                ReadOnlyCollection<LogicInt> varReqs,
                ReadOnlyCollection<StateModifier> stateModifiers)
            {
                StateProvider = stateProvider;
                TermReqs = termReqs;
                VarReqs = varReqs;
                StateModifiers = stateModifiers;
            }

            internal ReadOnlyConjunction(StatePath p, Reqs r)
            {
                StateProvider = p.stateProvider;
                StateModifiers = new(p.stateModifiers);
                r.GetReadOnlyData(out TermReqs, out VarReqs);
            }

            /// <summary>
            /// Produces an expression logically equivalent to the <see cref="ReadOnlyConjunction"/>.
            /// </summary>
            public Expression<LogicExpressionType> ToExpression()
            {
                LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
                return builder.ApplyInfixOperatorLeftAssoc(ToConjunctionArgs().DefaultIfEmpty(builder.NameAtom("NONE")), builder.Op(LogicOperatorProvider.AND));
            }

            /// <summary>
            /// Enumerates expressions which, ANDed together, give an expression logically equivalent to the <see cref="ReadOnlyConjunction"/>.
            /// <br/>Output does not contain AndExpressions or OrExpressions.
            /// </summary>
            public IEnumerable<Expression<LogicExpressionType>> ToConjunctionArgs()
            {
                if (StateProvider is not null) yield return StateProvider.ToExpression();
                foreach (TermValue tv in TermReqs) yield return tv.ToExpression();
                foreach (LogicInt li in VarReqs) yield return li.ToExpression();
                foreach (StateModifier sm in StateModifiers) yield return sm.ToExpression();
            }
        }
    }
}

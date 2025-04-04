using RandomizerCore.Logic.StateLogic;
using RandomizerCore.StringLogic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

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

        internal DNFLogicDef(Func<DNFLogicDef, StatePath[]> pathGenerator, LogicManager lm, string name, string infixSource)
            : base(name, infixSource)
        {
            this.lm = lm;
            this.paths = pathGenerator(this);
            Profiling.EmitMetric("DNFLogicDef.PathCount", paths.Length);
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

        public IEnumerable<TermToken>? GetFirstSuccessfulConjunction(ProgressionManager pm)
        {
            for (int j = 0; j < paths.Length; j++)
            {
                if (paths[j].GetFirstSuccessfulConjunction(pm) is IEnumerable<TermToken> c) return c;
            }
            return null;
        }

        public IEnumerable<IEnumerable<TermToken>> GetAllSuccessfulConjunctions(ProgressionManager pm)
        {
            for (int j = 0; j < paths.Length; j++)
            {
                foreach (IEnumerable<TermToken> c in paths[j].GetAllSuccessfulConjunctions(pm)) yield return c;
            }
        }

        public override IEnumerable<LogicToken> ToTokenSequence()
        {
            if (paths.Length == 0) return [ConstToken.False];
            return RPN.OperateOver(Enumerable.Range(0, paths.Length).Select(j => paths[j].ToTokenSequence()), OperatorToken.OR);
        }

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

            public IEnumerable<TermToken> ToTermTokenSequence(LogicManager lm)
            {
                foreach (TermValue tv in termReqs)
                {
                    if (tv.Value == 1) yield return lm.LP.GetTermToken(tv.Term.Name);
                    else yield return lm.LP.GetComparisonToken(ComparisonType.GT, tv.Term.Name, (tv.Value - 1).ToString());
                }
                foreach (LogicInt li in varReqs) yield return lm.LP.GetTermToken(li.Name);
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

            private bool EvaluateStateModifiersDiscardRec(ProgressionManager pm, StateUnion? input)
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

            private IEnumerable<TermToken> ClauseToTermTokenSequence(Reqs r) => ClauseToTermTokenSequence(r, stateModifiers.Select(s => parent.lm.LP.GetTermToken(s.Name)));

            private IEnumerable<TermToken> ClauseToTermTokenSequence(Reqs r, IEnumerable<TermToken> suffix)
            {
                LogicManager lm = parent.lm;
                IEnumerable<TermToken> tts = r.ToTermTokenSequence(lm).Concat(suffix);
                if (stateProvider is not null) tts = tts.Prepend(lm.LP.GetTermToken(stateProvider.Name));
                return tts.DefaultIfEmpty(ConstToken.True);
            }

            public IEnumerable<IEnumerable<TermToken>> ToTermTokenSequences()
            {
                LogicManager lm = parent.lm;
                List<TermToken> suffix = stateModifiers.Select(sm => lm.LP.GetTermToken(sm.Name)).ToList();
                return reqs.Select(r => ClauseToTermTokenSequence(r, suffix));
            }

            public IEnumerable<LogicToken> ToTokenSequence() => RPN.OperateOver(ToTermTokenSequences().Select(s => RPN.OperateOver(s, OperatorToken.AND)), OperatorToken.OR);

            public IEnumerable<TermToken>? GetFirstSuccessfulConjunction(ProgressionManager pm)
            {
                if (stateProvider?.GetInputState(parent, pm) is StateUnion input
                    && EvaluateAnyReqs(pm, out Reqs result)
                    && EvaluateStateModifiersDiscardRec(pm, input))
                {
                    return ClauseToTermTokenSequence(result);
                }
                return null;
            }

            public IEnumerable<IEnumerable<TermToken>> GetAllSuccessfulConjunctions(ProgressionManager pm)
            {
                if (stateProvider?.GetInputState(parent, pm) is StateUnion input
                    && EvaluateAllReqs(pm, out IEnumerable<Reqs> result)
                    && EvaluateStateModifiersDiscardRec(pm, input))
                {
                    return result.Select(ClauseToTermTokenSequence);
                }
                return Enumerable.Empty<IEnumerable<TermToken>>();
            }
        }
    }
}

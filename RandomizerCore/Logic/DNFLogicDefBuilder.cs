using RandomizerCore.Logic.StateLogic;
using RandomizerCore.StringLogic;
using System.Runtime.CompilerServices;
using System.Xml.Linq;
using OperatorToken = RandomizerCore.StringLogic.OperatorToken;

namespace RandomizerCore.Logic
{
    internal class DNFLogicDefBuilder
    {
        private readonly DNFConverter converter;
        private readonly StatePathBuilder pathBuilder;
        private readonly LogicManager lm;
        private readonly Dictionary<StatePathKey, List<StatePathData>> pathDataLookup = new();

        public DNFLogicDefBuilder(LogicManager lm)
        {
            this.lm = lm;
            pathBuilder = new(lm);
            converter = new();
        }

        public DNFLogicDef CreateDNFLogicDef(string name, string infix, List<LogicToken> tokens)
        {
            try
            {
                new TokenExpander(name, tokens, lm).Expand();
                converter.Convert(tokens);
                List<List<TermToken>> res = converter.Result;
                if (res.Count > 200)
                {
                    Log($"Warning - DNF for {name} expanded to {res.Count} clauses.");
                }

                //SequenceTree<ILogicVariable, StatePathData> statePathTree = new();
                pathBuilder.SetSource(name);
                pathDataLookup.Clear();

                foreach (List<TermToken> l in res)
                {
                    if (pathBuilder.TryCreatePathData(l, out StatePathData data)) // note: paths containing false const tokens are eliminated here
                    {
                        if (!pathDataLookup.TryGetValue(data.key, out List<StatePathData> dataList))
                        {
                            pathDataLookup.Add(data.key, dataList = []);
                        }
                        dataList.Add(data);
                    }
                }

                DNFLogicDef result = new(CreateClauses, lm, name, infix);
                Profiling.EmitMetric("CreateDNFLogicDef.ResultingTermCount", result.GetTerms().Count());
                return result;

                DNFLogicDef.StatePath[] CreateClauses(DNFLogicDef parent)
                {
                    DNFLogicDef.StatePath[] paths = new DNFLogicDef.StatePath[pathDataLookup.Count];
                    int i = 0;
                    foreach (List<StatePathData> l in pathDataLookup.Values)
                    {
                        StatePathData data = l[0];
                        DNFLogicDef.Reqs[] reqs = l.Select(spd => spd.reqs).ToArray();
                        paths[i++] = new(reqs, data.stateProvider, data.stateModifiers, parent);
                    }

                    bool hasState = paths.Any(p => p.stateProvider is not null);
                    if (hasState && paths.Any(p => p.stateProvider is null))
                    {
                        Log($"Warning - DNF for {name} contains clause with missing state provider.");
                    }

                    return paths;
                }
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error creating logic def for {name} with logic {infix}.", e);
            }
        }

        public void Trim()
        {
            converter.Trim();
        }

        public void Reset()
        {
            pathBuilder.Clear();
        }

        internal readonly struct TokenExpander
        {
            private readonly string name;
            private readonly List<LogicToken> tokens;
            private readonly LogicManager lm;

            public TokenExpander(string name, List<LogicToken> tokens, LogicManager lm)
            {
                this.name = name;
                this.tokens = tokens;
                this.lm = lm;
            }

            public void Expand()
            {
                for (int i = tokens.Count - 1; i >= 0; i--)
                {
                    ExpandToken(i);
                }
            }

            private void ExpandToken(int index)
            {
                switch (tokens[index])
                {
                    case ReferenceToken rt:
                        {
                            lm.ResolveLogicDefReference(name, rt.Target); // attempts to create the referenced logic def if it does not yet exist, throws for missing or cyclic references.
                        }
                        break;
                    case ProjectedToken { Inner: ReferenceToken rt }:
                        {
                            lm.ResolveLogicDefReference(name, rt.Target);
                        }
                        break;
                    case MacroToken mt:
                        {
                            lm._cycleDetector.PushLogic(mt.Name);
                            ExpandTokenRange(index, mt.Value.Tokens);
                            lm._cycleDetector.PopLogic(mt.Name);
                        }
                        break;
                    case CoalescingToken qt:
                        tokens[index] = IsValidToken(qt.Left) ? qt.Left : qt.Right;
                        break;
                }
            }

            private void ExpandTokenRange(int index, IEnumerable<LogicToken> replaceSequence)
            {
                int count = tokens.Count;
                tokens.RemoveAt(index);
                tokens.InsertRange(index, replaceSequence);
                count = tokens.Count - count;

                for (int i = index + count; i >= index; i--)
                {
                    ExpandToken(i);
                }
            }

            private bool IsValidToken(LogicToken lt)
            {
                return lt switch
                {
                    OperatorToken => true,
                    SimpleToken st => lm.IsTermOrVariable(st.Name),
                    ProjectedToken { Inner: SimpleToken st } => lm.IsTermOrVariable(st.Name),
                    ComparisonToken ct => lm.IsTermOrVariable(ct.Left) && lm.IsTermOrVariable(ct.Right),
                    ConstToken => true,
                    MacroToken mt => mt.Source?.GetMacro(mt.Name) is not null,
                    CoalescingToken qt => IsValidToken(qt.Left) || IsValidToken(qt.Right),
                    _ => false,
                };
            }
        }

        internal readonly struct StatePathKey : IEquatable<StatePathKey>
        {
            public readonly IStateProvider? stateProvider;
            public readonly StateModifier[] stateModifiers;

            public StatePathKey(IStateProvider? stateProvider, StateModifier[] stateModifiers)
            {
                this.stateProvider = stateProvider;
                this.stateModifiers = stateModifiers;
            }

            public bool Equals(StatePathKey other)
            {
                if (!ReferenceEquals(stateProvider, other.stateProvider) || stateModifiers.Length != other.stateModifiers.Length) return false;
                for (int i = 0; i < stateModifiers.Length; i++) if (!ReferenceEquals(stateModifiers[i], other.stateModifiers[i])) return false;
                return true;
            }

            public override int GetHashCode()
            {
                int hash = RuntimeHelpers.GetHashCode(stateProvider);
                for (int i = 0; i < stateModifiers.Length; i++) hash = ((hash << 5) + hash) ^ RuntimeHelpers.GetHashCode(stateModifiers[i]);
                return hash;
            }
        }

        internal class StatePathBuilder
        {
            private IStateProvider? stateProvider = null;
            private readonly List<TermValue> termReqs = new();
            private readonly List<LogicInt> varReqs = new();
            private readonly List<StateModifier> stateModifiers = new();
            private string source;
            private readonly LogicManager lm;

            public StatePathBuilder(LogicManager lm)
            {
                this.lm = lm;
            }

            private void AddTermReq(Term t, int strictLowerBound) => termReqs.Add(new(t, strictLowerBound + 1));
            private void AddTermReq(Term t) => termReqs.Add(new(t, 1));
            private void AddVarReq(LogicInt li) => varReqs.Add(li);
            private void AddStateModifier(StateModifier sm) => stateModifiers.Add(sm);
            private void SetStateProvider(IStateProvider sp)
            {
                if (stateProvider is null)
                {
                    stateProvider = sp;
                }
                else
                {
                    WarnDuplicateStateProvider(sp);
                    if (sp is Term t) AddTermReq(t);
                    else if (sp is LogicInt li) AddVarReq(li);
                    else throw new NotSupportedException("Unrecgonized duplicate IStateProvider " + sp.Name);
                }
            }
            public void SetSource(string source)
            {
                this.source = source;
            }

            public void ApplyToken(TermToken tt)
            {
                switch (tt)
                {
                    case SimpleToken st:
                        ApplySimpleToken(st);
                        break;
                    case ComparisonToken ct:
                        ApplyComparisonToken(ct);
                        break;
                    case ReferenceToken rt:
                        ApplyReferenceToken(rt);
                        break;
                    case ConstToken:
                        break;
                    case ProjectedToken pt:
                        ApplyProjectedToken(pt);
                        break;
                    default: throw new NotSupportedException();
                }
            }

            public void ApplySimpleToken(SimpleToken st)
            {
                if (lm.GetTerm(st.Name) is Term t)
                {
                    if (t.Type == TermType.State) SetStateProvider(t);
                    else AddTermReq(t);
                }
                else
                {
                    switch (lm.GetVariableStrict(st.Name))
                    {
                        case StateModifier sm:
                            AddStateModifier(sm);
                            break;
                        case StateAccessVariable sav:
                            throw StateAccessModifierException(sav);
                        case StateProvider sp:
                            SetStateProvider(sp);
                            break;
                        case LogicInt li:
                            AddVarReq(li);
                            break;
                        case LogicVariable lv:
                            throw new ArgumentException($"Found unrecognized variable {lv.Name} of type {lv.GetType()}");
                    }
                }
            }

            public void ApplyComparisonToken(ComparisonToken ct)
            {
                if (ct.ComparisonType == ComparisonType.GT && lm.GetTerm(ct.Left) is Term termL && int.TryParse(ct.Right, out int intR))
                {
                    AddTermReq(termL, intR);
                }
                else
                {
                    ILogicVariable left = (ILogicVariable)lm.GetTerm(ct.Left) ?? lm.GetVariableStrict(ct.Left);
                    ILogicVariable right = (ILogicVariable)lm.GetTerm(ct.Right) ?? lm.GetVariableStrict(ct.Right);
                    int op = ct.ComparisonType switch
                    {
                        ComparisonType.LT => -1,
                        ComparisonType.GT => 1,
                        _ => 0,
                    };

                    if (left is StateAccessVariable savL)
                    {
                        AddStateModifier(new StateAccessorWrapper(ct.Write(), savL, right, op));
                    }
                    else if (right is StateAccessVariable savR)
                    {
                        ct = ct with
                        {
                            Left = ct.Right,
                            Right = ct.Left,
                            ComparisonType = ct.ComparisonType switch
                            {
                                ComparisonType.LT => ComparisonType.GT,
                                ComparisonType.GT => ComparisonType.LT,
                                _ => ComparisonType.EQ,
                            }
                        };
                        AddStateModifier(new StateAccessorWrapper(ct.Write(), savR, left, op));
                    }
                    else
                    {
                        AddVarReq(new ComparisonVariable(ct.Write(), (ILogicInt)left, (ILogicInt)right, op));
                    }
                }
            }

            public void ApplyReferenceToken(ReferenceToken rt)
            {
                LogicDef ld = lm.GetLogicDefStrict(rt.Target);
                if (ld is StateLogicDef sld)
                {
                    SetStateProvider(new LogicStateProvider(sld));
                }
                else
                {
                    AddVarReq(new LogicDefVariable(ld));
                }
            }

            public void ApplyProjectedToken(ProjectedToken pt)
            {
                if (pt.Inner is ReferenceToken rt)
                {
                    AddVarReq(new LogicDefVariable(lm.GetLogicDefStrict(rt.Target)));
                }
                else if (pt.Inner is SimpleToken st)
                {
                    if (lm.GetTerm(st.Name) is Term t)
                    {
                        AddTermReq(t);
                    }
                    else
                    {
                        AddVarReq((StateProvider)lm.GetVariableStrict(st.Name));
                    }
                }
            }

            public bool TryCreatePathData(IEnumerable<TermToken> tts, out StatePathData data)
            {
                foreach (TermToken tt in tts)
                {
                    if (tt is ConstToken { Value: false })
                    {
                        Clear();
                        data = default;
                        return false;
                    }
                    ApplyToken(tt);
                }
                data = FinalizeDataAndClear();
                return true;
            }

            public void Clear()
            {
                stateProvider = null;
                termReqs.Clear();
                varReqs.Clear();
                stateModifiers.Clear();
            }

            private StatePathData FinalizeDataAndClear()
            {
                StatePathData result = new(new DNFLogicDef.Reqs(termReqs.ToArray(), varReqs.ToArray()), stateProvider, stateModifiers.ToArray());
                Clear();
                return result;
            }

            private Exception StateAccessModifierException(StateAccessVariable sav)
            {
                return new InvalidOperationException($"Found StateAccessVariable {sav.Name} in logic. Regular logic requires SAVs to be wrapped in a comparison, to avoid common errors with state partial order.");
            }

            private void WarnDuplicateStateProvider(IStateProvider sp)
            {
                Log($"Warning - DNF for {source} contains a clause with ambiguous state providers: {stateProvider.Name}, {sp.Name}");
            }
        }

        internal readonly struct StatePathData
        {
            public readonly DNFLogicDef.Reqs reqs;
            public readonly StatePathKey key;
            public IStateProvider? stateProvider => key.stateProvider;
            public StateModifier[] stateModifiers => key.stateModifiers;

            public StatePathData(DNFLogicDef.Reqs reqs, IStateProvider? stateProvider, StateModifier[] stateModifiers)
            {
                this.reqs = reqs;
                this.key = new(stateProvider, stateModifiers);
            }
        }
    }
}

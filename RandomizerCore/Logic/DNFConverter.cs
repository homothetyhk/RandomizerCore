using RandomizerCore.Logic.StateLogic;
using RandomizerCore.Extensions;
using static RandomizerCore.Logic.DNFLogicDef;
using RandomizerCore.StringParsing;
using RandomizerCore.StringLogic;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// Class for converting RPN logic to DNF logic, which uses pooled lists to reduce memory usage.
    /// </summary>
    internal class DNFConverter
    {
        private readonly Stack<StatePathBuilder> listPool = [];
        private readonly Stack<List<StatePathBuilder>> outerListPool = [];

        // used in DNFLogicDef constructor
        internal StatePath[] CreateLogicData(string name, Expression<LogicExpressionType> expr, LogicManager lm, DNFLogicDef target)
        {
            List<StatePathBuilder> expansion = ProcessLogic(name, expr, lm);
            if (expansion.Count > 200)
            {
                Log($"Warning - DNF for {name} expanded to {expansion.Count} clauses.");
            }

            Dictionary<StatePathBuilder.Key, List<StatePathBuilder>> sorter = [];
            foreach (StatePathBuilder sp in expansion)
            {
                StatePathBuilder.Key key = sp.ToKey();
                if (!sorter.TryGetValue(key, out List<StatePathBuilder> l))
                {
                    sorter.Add(key, l = OuterListFromPool());
                }
                l.Add(sp);
            }

            StatePath[] arr = [.. sorter.Select(kvp =>
                {
                    Reqs[] reqs = [.. kvp.Value.Select(s => new Reqs([.. s.TermReqs], [.. s.VarReqs]))];
                    IStateProvider? stateProvider = kvp.Key.stateProvider;
                    StateModifier[] stateModifiers = [.. kvp.Key.stateModifiers];
                    return new StatePath(reqs, stateProvider, stateModifiers, target);
                })];

            foreach (var ll in sorter.Values) RecycleAll(ll);
            RecycleOuter(expansion);

            CheckStateLogicConsistency(name, arr);

            return arr;
        }

        // cycle detection is handled in LM.InitMacro
        private List<StatePathBuilder> ProcessMacro(string name, Expression<LogicExpressionType> expr, LogicManager lm)
        {
            try
            {
                return Process(name, expr, lm);
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error processing macro {name} to interpreted logic", e);
            }
        }

        private List<StatePathBuilder> ProcessLogic(string name, Expression<LogicExpressionType> expr, LogicManager lm)
        {
            try
            {
                return Process(name, expr, lm);
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error processing LogicDef {name} to interpreted logic", e);
            }
        }

        private List<StatePathBuilder> Process(string name, Expression<LogicExpressionType> expr, LogicManager lm)
        {
            switch (expr)
            {
                case GroupingExpression<LogicExpressionType> g: return Process(name, g.Nested, lm);
                case LogicAtomExpression { Token.Content: string atomName }:
                    if (lm.GetMacro(atomName) is MacroDef macro)
                    {
                        return ProcessMacro(atomName, macro.Logic.Expr, lm);
                    }
                    else
                    {
                        return Atom(lm.GetTermOrVariableStrict(atomName));
                    }
                case NumberLiteralExpression { ConstValue: int intAtom }:
                    return Const(intAtom > 0);
                case BoolLiteralExpression { ConstValue: bool boolAtom }:
                    return Const(boolAtom);
                case AndExpression a:
                    return And(name, a.Left, a.Right, lm); // Process called inside to allow short-circuiting.
                case OrExpression o:
                    return Or(Process(name, o.Left, lm), Process(name, o.Right, lm));
                case CoalesceExpression q:
                    return q.Left.IsDefined(lm) ? Process(name, q.Left, lm) : Process(name, q.Right, lm);
                case ComparisonExpression c:
                    {
                        (string?, int) left = c.Left.ToComparisonOperand(lm);
                        (string?, int) right = c.Right.ToComparisonOperand(lm);
                        int op = c.Operator.Definition.Operator switch
                        {
                            LogicOperatorProvider.GT => 1,
                            LogicOperatorProvider.EQ => 0,
                            LogicOperatorProvider.LT => -1,
                            _ => throw new NotImplementedException()
                        };
                        
                        return (left, right) switch
                        {
                            ((null, int i1), (null, int i2)) => Const(Math.Sign(i1.CompareTo(i2)) == op),
                            ((string s1, _), (null, int i2)) when op == 1 && lm.GetTerm(s1) is Term t1 => Atom(t1, i2),
                            ((string s1, _), (null, int i2)) => CompareVariables(lm.GetTermOrVariableStrict(s1), new ConstantInt(i2), op),
                            ((null, int i1), (string s2, _)) => CompareVariables(new ConstantInt(i1), lm.GetTermOrVariableStrict(s2), op),
                            ((string s1, _), (string s2, _)) => CompareVariables(lm.GetTermOrVariableStrict(s1), lm.GetTermOrVariableStrict(s2), op),
                        };
                    }
                case ReferenceExpression r:
                    {
                        string reference = r.Operand.ToIdentifier(lm);
                        LogicDef ld = lm.GetLogicDefStrict(reference);
                        return Atom(ld is StateLogicDef sld ? new LogicStateProvider(sld) : new LogicDefVariable(ld, reference: true));
                    }
                case ProjectionExpression p:
                    {
                        Expression<LogicExpressionType> pOperand = ReduceProjectionOperand(p.Operand, lm);
                        
                        if (pOperand is ReferenceExpression pr)
                        {
                            string reference = pr.Operand.ToIdentifier(lm);
                            LogicDef ld = lm.GetLogicDefStrict(reference);
                            return Atom(new LogicDefVariable(ld, projected: true, reference: true));
                        }
                        else if (pOperand is LogicAtomExpression a)
                        {
                            string atomName = a.ToIdentifier(lm);
                            return Atom(new ProjectedStateProvider((IStateProvider)lm.GetTermOrVariableStrict(atomName)));
                        }
                        else
                        {
                            return Atom(new LogicDefVariable(ConvertSubexpressionToLogic(name, pOperand, lm), projected: true));
                        }
                    }
                default:
                    throw new NotImplementedException(expr.GetType().Name);
            }
        }

        // eliminates groupings, coalescings, and macros from an expression.
        // used in projection expressions to determine whether the operand reduces to exactly a reference expression.
        // simple projected references can be optimized better than arbitrary projected logic.
        private static Expression<LogicExpressionType> ReduceProjectionOperand(Expression<LogicExpressionType> expr, LogicManager lm) => expr switch
        {
            GroupingExpression<LogicExpressionType> g => ReduceProjectionOperand(g.Nested, lm),
            CoalesceExpression q => q.Left.IsDefined(lm) ? ReduceProjectionOperand(q.Left, lm) : ReduceProjectionOperand(q.Right, lm),
            LogicAtomExpression { Token.Content: string n } => lm.GetMacro(n) is MacroDef macro ? ReduceProjectionOperand(macro.Logic.Expr, lm) : expr,
            _ => expr,
        };

        private string GetSubexpressionName(string parentName, string infix)
        {
            return $"{parentName}-Subexpression`{infix}`";
        }

        private DNFLogicDef ConvertSubexpressionToLogic(string parentName, Expression<LogicExpressionType> expr, LogicManager lm)
        {
            string infix = expr.Print();
            string name = GetSubexpressionName(parentName, infix);
            return new(name, infix, expr, lm);
        }

        private List<StatePathBuilder> Atom(ILogicVariable variable)
        {
            List<StatePathBuilder> ll = OuterListFromPool();
            StatePathBuilder l = InnerListFromPool();
            l.Add(variable);
            ll.Add(l);
            return ll;
        }

        private List<StatePathBuilder> Atom(Term t, int exLowerBound)
        {
            List<StatePathBuilder> ll = OuterListFromPool();
            StatePathBuilder l = InnerListFromPool();
            l.Add(t, exLowerBound);
            ll.Add(l);
            return ll;
        }

        private List<StatePathBuilder> CompareVariables(ILogicVariable left, ILogicVariable right, int op)
        {
            if (left is StateAccessVariable || right is StateAccessVariable)
            {
                StateAccessVariable savL = left as StateAccessVariable ?? new SAVFromLogicInt((ILogicInt)left);
                StateAccessVariable savR = right as StateAccessVariable ?? new SAVFromLogicInt((ILogicInt)right);
                StateModifier sm = new StateModifierFromSAV(savL, savR, op);
                return Atom(sm);
            }
            else
            {
                ComparisonVariable c = new((ILogicInt)left, (ILogicInt)right, op);
                return Atom(c);
            }
        }

        private List<StatePathBuilder> Const(bool value)
        {
            if (value)
            {
                List<StatePathBuilder> ll = OuterListFromPool();
                StatePathBuilder l = InnerListFromPool();
                ll.Add(l);
                return ll;
            }
            else
            {
                return OuterListFromPool();
            }
        }

        private List<StatePathBuilder> And(string name, Expression<LogicExpressionType> leftExpr, Expression<LogicExpressionType> rightExpr, LogicManager lm)
        {
            var left = Process(name, leftExpr, lm);
            if (left.Count == 0)
            {
                return left; // FALSE
            }
            var right = Process(name, rightExpr, lm);
            if (right.Count == 0)
            {
                RecycleAll(left);
                return right; // FALSE
            }

            if (right.Count == 1)
            {
                var r = right[0];
                for (int i = 0; i < left.Count; i++)
                {
                    left[i].AndWith(r);
                }
                RecycleAll(right);
                return left;
            }
            if (left.Count == 1)
            {
                StatePathBuilder l = left[0];
                RecycleOuter(left);
                for (int i = 0; i < right.Count; i++)
                {
                    StatePathBuilder s = CloneFromPoolOrSelf(l, i, right.Count);
                    s.AndWith(right[i]);
                    Recycle(right[i]);
                    right[i] = s;
                }
                return right;
            }

            List<StatePathBuilder> result = OuterListFromPool();
            for (int i = 0; i < left.Count; i++)
            {
                for (int j = 0; j < right.Count; j++)
                {
                    StatePathBuilder l = CloneFromPoolOrSelf(left[i], j, right.Count);
                    l.AndWith(right[j]);
                    result.Add(l);
                }
            }
            RecycleAll(right);
            RecycleOuter(left);
            return result;
        }


        private List<StatePathBuilder> Or(List<StatePathBuilder> left, List<StatePathBuilder> right)
        {
            left.AddRange(right);
            RecycleOuter(right);
            return left;
        }

        private StatePathBuilder InnerListFromPool() => listPool.TryPop(out StatePathBuilder s) ? s : new();
        private List<StatePathBuilder> OuterListFromPool() => outerListPool.TryPop(out List<StatePathBuilder> l) ? l : [];

        private StatePathBuilder CloneFromPoolOrSelf(StatePathBuilder orig, int index, int count)
        {
            if (count - index == 1) return orig;
            StatePathBuilder l = InnerListFromPool();
            orig.CopyTo(l);
            return l;
        }

        private void Recycle(StatePathBuilder list)
        {
            list.Clear();
            listPool.Push(list);
        }
        private void RecycleAll(List<StatePathBuilder> list)
        {
            for (int i = 0; i < list.Count; i++)
            {
                Recycle(list[i]);
            }
            list.Clear();
            outerListPool.Push(list);
        }
        private void RecycleOuter(List<StatePathBuilder> list)
        {
            list.Clear();
            outerListPool.Push(list);
        }

        /// <summary>
        /// Reduces the number of pooled lists.
        /// </summary>
        public void Trim()
        {
            outerListPool.Clear();
            listPool.Clear();
        }


        private static void CheckStateLogicConsistency(string name, StatePath[] paths)
        {
            bool hasState = paths.Any(p => p.stateProvider is not null);
            if (hasState && paths.Any(p => p.stateProvider is null))
            {
                Log($"Warning - DNF for {name} contains clause with missing state provider.");
#if DEBUG
                foreach (var p in paths)
                {
                    if (p.stateProvider is null) LogDebug($"        {p.ToExpression().Print()}");
                }
#endif
            }
            if (paths.Any(p => p.stateProvider is null && p.stateModifiers.Length > 0))
            {
                throw new ArgumentException("Found state modifiers without an associated state provider.");
            }
        }
    }
}

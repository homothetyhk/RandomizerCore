using RandomizerCore.Logic.StateLogic;
using RandomizerCore.StringLogic;
using RandomizerCore.StringParsing;

namespace RandomizerCore.Logic
{
    internal readonly struct RPNLogicDefBuilder
    {
        public readonly string name;
        public readonly LogicClause logic;
        public readonly List<RPNLogicEntry> entries;
        public readonly RPNLogicType type;
        public readonly LogicManager lm;

        public enum RPNLogicType
        {
            Stateless, // state modifiers and state access variables are invalid, state providers are evaluated as logic ints.
            SingleState, // state modifiers are invalid, state access variables can be used in comparisons, state providers are evaluated as logic ints.
        }

        public RPNLogicDefBuilder(string name, LogicClause logic, LogicManager lm, RPNLogicType type)
        {
            this.lm = lm;
            this.name = name;
            this.logic = logic;
            this.type = type;
            this.entries = [];
            try
            {
                Consume(logic.Expr, lm);
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error parsing {name} as {type} RPN logic.", e);
            }
        }


        private void Consume(Expression<LogicExpressionType> expr, LogicManager lm)
        {
            switch (expr.TrimParens())
            {
                case LogicAtomExpression a:
                    string value = a.Token.Content;
                    if (lm.GetMacro(value) is MacroDef macro)
                    {
                        Consume(macro.Logic.Expr, lm);
                    }
                    else
                    {
                        ILogicVariable ilv = lm.GetTermOrVariableStrict(value);
                        Atom(ilv);
                    }
                    return;

                case BoolLiteralExpression a:
                    entries.Add(a.ConstValue ? RPNLogicEntry.True : RPNLogicEntry.False);
                    return;

                case NumberLiteralExpression a:
                    entries.Add(a.ConstValue > 0 ? RPNLogicEntry.True : RPNLogicEntry.False);
                    break;

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

                        switch ((left, right))
                        {
                            case ((null, int i1), (null, int i2)):
                                Const(Math.Sign(i1.CompareTo(i2)) == op);
                                return;
                            case ((string s1, _), (null, int i2)) when op == 1 && lm.GetTerm(s1) is Term t1:
                                Atom(t1, i2 + 1); // termReqs uses inclusive lower bound
                                return;
                            case ((string s1, _), (null, int i2)):
                                CompareVariables(lm.GetTermOrVariableStrict(s1), new ConstantInt(i2), op);
                                return;
                            case ((null, int i1), (string s2, _)):
                                CompareVariables(new ConstantInt(i1), lm.GetTermOrVariableStrict(s2), op);
                                return;
                            case ((string s1, _), (string s2, _)):
                                CompareVariables(lm.GetTermOrVariableStrict(s1), lm.GetTermOrVariableStrict(s2), op);
                                return;
                            default: throw new NotImplementedException();
                        };
                    }

                case CoalesceExpression q:
                    Consume(q.Left.IsDefined(lm) ? q.Left : q.Right, lm);
                    return;

                // AND and OR have special handling to reduce FALSE at this stage.
                // We do not reduce TRUE here, since state logic requires special handling.
                case AndExpression a:
                    {
                        int startCount = entries.Count;
                        Consume(a.Left, lm);
                        int sepCount = entries.Count;
                        Consume(a.Right, lm);

                        bool leftFalse = entries[startCount].IsConstFalse;
                        bool rightFalse = entries[sepCount].IsConstFalse;

                        if (leftFalse || rightFalse) // AND expression which reduces to FALSE
                        {
                            entries.RemoveRange(startCount, entries.Count - startCount);
                            entries.Add(RPNLogicEntry.False);
                        }
                        else // standard AND expression
                        {
                            entries.Add(RPNLogicEntry.And);
                        }
                    }
                    return;

                case OrExpression o:
                    {
                        int startCount = entries.Count;
                        Consume(o.Left, lm);
                        int sepCount = entries.Count;
                        Consume(o.Right, lm);

                        bool leftFalse = entries[startCount].IsConstFalse;
                        bool rightFalse = entries[sepCount].IsConstFalse;

                        switch (leftFalse, rightFalse)
                        {
                            case (false, false): // standard OR expression
                                entries.Add(RPNLogicEntry.Or);
                                return;
                            case (true, false): // keep only RIGHT
                                entries.RemoveAt(startCount);
                                return;
                            case (false, true): // keep only LEFT
                            case (true, true): // reduce to FALSE
                                entries.RemoveAt(sepCount);
                                return;
                        }
                    }
                case ProjectionExpression:
                case ReferenceExpression:
                default:
                    throw new NotImplementedException(expr.GetType().Name);
            }
        }

        private void Atom(ILogicVariable lv)
        {
            if (lv is Term t)
            {
                entries.Add(new(t, 1));
                return;
            }

            switch (type)
            {
                case RPNLogicType.Stateless:
                    {
                        switch (lv)
                        {
                            case LogicInt: 
                                break;
                            default: 
                                throw UnsupportedVariableError(lv);
                        }
                        entries.Add(new(lv));
                        return;
                    }
                case RPNLogicType.SingleState:
                    {
                        switch (lv)
                        {
                            case LogicInt:
                            case StateAccessVariable:
                                break;
                            default: 
                                throw UnsupportedVariableError(lv);
                        }
                        entries.Add(new(lv));
                        return;
                    }
            }
        }

        private void Atom(Term t, int incLowerBound)
        {
            entries.Add(new(t, incLowerBound));
        }

        private void Const(bool value)
        {
            entries.Add(value ? RPNLogicEntry.True : RPNLogicEntry.False);
        }

        private void CompareVariables(ILogicVariable left, ILogicVariable right, int op)
        {
            switch (type)
            {
                case RPNLogicType.Stateless:
                    Atom(new ComparisonVariable((ILogicInt)left, (ILogicInt)right, op));
                    return;
                case RPNLogicType.SingleState:
                    if (left is StateAccessVariable || right is StateAccessVariable)
                    {
                        StateAccessVariable savL = left as StateAccessVariable ?? new SAVFromLogicInt((ILogicInt)left);
                        StateAccessVariable savR = right as StateAccessVariable ?? new SAVFromLogicInt((ILogicInt)right);
                        Atom(new SAVComparisonVariable(savL, savR, op));
                    }
                    else
                    {
                        Atom(new ComparisonVariable((ILogicInt)left, (ILogicInt)right, op));
                    }
                    return;
            }


            if (left is StateAccessVariable || right is StateAccessVariable)
            {
                StateAccessVariable savL = left as StateAccessVariable ?? new SAVFromLogicInt((ILogicInt)left);
                StateAccessVariable savR = right as StateAccessVariable ?? new SAVFromLogicInt((ILogicInt)right);
                StateModifier sm = new StateModifierFromSAV(savL, savR, op);
                Atom(sm);
            }
            else
            {
                ComparisonVariable c = new((ILogicInt)left, (ILogicInt)right, op);
                Atom(c);
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

        private Exception UnsupportedVariableError(ILogicVariable lv)
        {
            return new NotSupportedException($"Found unsupported variable {lv.Name} of type {lv.GetType().Name}");
        }
    }
}

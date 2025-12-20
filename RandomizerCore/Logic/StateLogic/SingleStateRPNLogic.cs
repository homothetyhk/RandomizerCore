using RandomizerCore.StringLogic;
using RandomizerCore.StringParsing;

namespace RandomizerCore.Logic.StateLogic
{
    internal class SingleStateRPNLogic : SingleStateLogic
    {
        public SingleStateRPNLogic(string name, LogicClause lc, LogicManager lm) : base(name, lc.ToInfix())
        {
            this.lm = lm;
            RPNLogicDefBuilder builder = new(name, lc, lm, RPNLogicDefBuilder.RPNLogicType.SingleState);
            logic = [.. builder.entries];
        }

        private readonly RPNLogicEntry[] logic;
        private readonly LogicManager lm;

        public override bool CanGet<T>(ProgressionManager pm, T state)
        {
            try
            {
                return Eval(pm, state);
            }
            catch (Exception e)
            {
                throw ThrowHelper(e);
            }
        }

        private bool Has<T>(ILogicVariable variable, int incLB, ProgressionManager pm, T state) where T : IState => variable switch
        {
            Term t => pm.Has(t, incLB),
            LogicInt li => li.GetValue(this, pm) >= incLB,
            StateAccessVariable sav => sav.GetValue(this, pm, state) >= incLB,
            _ => throw new NotImplementedException(variable.GetType().Name),
        };

        private bool Eval<T>(ProgressionManager pm, T state) where T : IState
        {
            Stack<bool> stack = [];
            foreach (RPNLogicEntry e in logic)
            {
                if (e.Variable is null)
                {
                    if (e.IsAnd)
                    {
                        bool argR = stack.Pop();
                        bool argL = stack.Pop();
                        return argL && argR;
                    }
                    else if (e.IsOr)
                    {
                        bool argR = stack.Pop();
                        bool argL = stack.Pop();
                        return argL || argR;
                    }
                    else if (e.IsConstFalse)
                    {
                        stack.Push(false);
                    }
                    else if (e.IsConstTrue)
                    {
                        stack.Push(true);
                    }
                }
                else
                {
                    stack.Push(Has(e.Variable, e.Value, pm, state));
                }
            }
            if (stack.Count != 0) throw new InvalidOperationException("Found extra operands in the stack after evaluation.");
            return stack.Pop();
        }

        private Exception ThrowHelper(Exception e)
        {
            return new InvalidOperationException($"Error evaluating {GetType().Name} {Name} with source {InfixSource}", e);
        }

        public override IEnumerable<Term> GetTerms()
        {
            for (int i = logic.Length - 1; i >= 0; i--)
            {
                switch (logic[i].Variable)
                {
                    case null: continue;
                    case Term t: 
                        yield return t; 
                        break;
                    case LogicVariable lv:
                        foreach (Term t in lv.GetTerms()) { yield return t; }
                        break;
                }
            }
        }

        public IEnumerable<StateField> GetStateFields()
        {
            for (int i = logic.Length - 1; i >= 0; i--)
            {
                switch (logic[i].Variable)
                {
                    case StateAccessVariable sav:
                        foreach (StateField sf in sav.GetStateFields()) { yield return sf; }
                        break;
                }
            }
        }

        public override Expression<LogicExpressionType> ToExpression()
        {
            try
            {
                return EvalExpression();
            }
            catch (Exception e)
            {
                throw ThrowHelper(e);
            }
        }

        private Expression<LogicExpressionType> EvalExpression()
        {
            Stack<Expression<LogicExpressionType>> stack = [];
            LogicExpressionBuilder builder = LogicExpressionUtil.Builder;
            foreach (RPNLogicEntry e in logic)
            {
                if (e.Variable is null)
                {
                    if (e.IsAnd)
                    {
                        Expression<LogicExpressionType> argR = stack.Pop();
                        Expression<LogicExpressionType> argL = stack.Pop();
                        stack.Push(builder.ApplyInfixOperator(argL, builder.Op(LogicOperatorProvider.AND), argR));
                    }
                    else if (e.IsOr)
                    {
                        Expression<LogicExpressionType> argR = stack.Pop();
                        Expression<LogicExpressionType> argL = stack.Pop();
                        stack.Push(builder.ApplyInfixOperator(argL, builder.Op(LogicOperatorProvider.OR), argR));
                    }
                    else if (e.IsConstTrue)
                    {
                        stack.Push(builder.NameAtom(true.ToString().ToUpper()));
                    }
                    else if (e.IsConstFalse)
                    {
                        stack.Push(builder.NameAtom(false.ToString().ToUpper()));
                    }
                    else throw new NotImplementedException();
                }
                else
                {
                    if (e.Variable is ComparisonVariable cv)
                    {
                        Expression<LogicExpressionType> left = builder.NameOrNumberAtom(cv.Left.Name);
                        Expression<LogicExpressionType> right = builder.NameOrNumberAtom(cv.Right.Name);
                        StringParsing.OperatorToken op = builder.Op(cv.Op switch
                        {
                            < 0 => "<", 
                            0 => "=", 
                            > 0 => ">"
                        });
                        stack.Push(builder.ApplyInfixOperator(left, op, right));
                    }
                    else if (e.Variable is SAVComparisonVariable savCV)
                    {
                        Expression<LogicExpressionType> left = builder.NameOrNumberAtom(savCV.Left.Name);
                        Expression<LogicExpressionType> right = builder.NameOrNumberAtom(savCV.Right.Name);
                        StringParsing.OperatorToken op = builder.Op(savCV.Op switch
                        {
                            < 0 => "<",
                            0 => "=",
                            > 0 => ">"
                        });
                        stack.Push(builder.ApplyInfixOperator(left, op, right));
                    }
                    else
                    {
                        Expression<LogicExpressionType> atom = builder.NameAtom(e.Variable.Name);
                        stack.Push(e.Value == 1
                                    ? atom
                                    : builder.ApplyInfixOperator(atom, builder.Op(LogicOperatorProvider.GT), builder.NumberAtom(e.Value)));
                    }
                }
            }
            if (stack.Count != 0) throw new InvalidOperationException("Found extra operands in the stack after evaluation.");
            return stack.Pop();
        }
    }
}

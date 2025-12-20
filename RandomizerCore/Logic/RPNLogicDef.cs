using RandomizerCore.StringLogic;
using RandomizerCore.StringParsing;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// A <see cref="LogicDef"/> which stores its contents in reverse Polish notation. Does not support state logic.
    /// </summary>
    public class RPNLogicDef : LogicDef
    {
        public RPNLogicDef(string name, LogicClause lc, LogicManager lm) : base(name, lc.ToInfix())
        {
            this.lm = lm;
            RPNLogicDefBuilder builder = new(name, lc, lm, RPNLogicDefBuilder.RPNLogicType.Stateless);
            this.logic = [.. builder.entries];
        }

        public RPNLogicDef(RPNLogicDef def) : base(def.Name, def.InfixSource)
        {
            this.lm = def.lm;
            this.logic = def.logic;
        }

        private readonly RPNLogicEntry[] logic;
        private readonly LogicManager lm;

        public override bool CanGet(ProgressionManager pm)
        {
            try
            {
                return Eval(pm);
            }
            catch (Exception e)
            {
                throw ThrowHelper(e);
            }
        }

        private bool Has(ILogicVariable variable, int incLB, ProgressionManager pm) => variable switch
        {
            Term t => pm.Has(t, incLB),
            LogicInt li => li.GetValue(this, pm) >= incLB,
            _ => throw new NotImplementedException(variable.GetType().Name),
        };

        private bool Eval(ProgressionManager pm)
        {
            Stack<bool> stack = [];
            foreach (var e in logic)
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
                    stack.Push(Has(e.Variable, e.Value, pm));
                }
            }
            if (stack.Count != 0) throw new InvalidOperationException("Found extra operands in the stack after evaluation.");
            return stack.Pop();
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
            var builder = LogicExpressionUtil.Builder;
            foreach (var e in logic)
            {
                if (e.Variable is null)
                {
                    if (e.IsAnd)
                    {
                        var argR = stack.Pop();
                        var argL = stack.Pop();
                        stack.Push(builder.ApplyInfixOperator(argL, builder.Op(LogicOperatorProvider.AND), argR));
                    }
                    else if (e.IsOr)
                    {
                        var argR = stack.Pop();
                        var argL = stack.Pop();
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

        private Exception ThrowHelper(Exception e)
        {
            return new InvalidOperationException($"Error evaluating {GetType().Name} {Name} with source {InfixSource}", e);
        }
    }
}

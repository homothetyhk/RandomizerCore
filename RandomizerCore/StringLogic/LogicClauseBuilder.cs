using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace RandomizerCore.StringLogic
{
    public class LogicClauseBuilder // TODO: Simpl, Subst, DNF
    {
        private readonly List<LogicToken> _tokens;
        public readonly ReadOnlyCollection<LogicToken> Tokens;

        /// <summary>
        /// Initializes an empty LCB. The only valid operation on an empty LCB is Append with a TermToken.
        /// </summary>
        public LogicClauseBuilder()
        {
            _tokens = new();
            Tokens = new(_tokens);
        }

        public LogicClauseBuilder(TermToken t)
        {
            _tokens = new();
            Tokens = new(_tokens);
            Append(t);
        }

        public LogicClauseBuilder(LogicClause c)
        {
            _tokens = new(c.Count);
            Tokens = new(_tokens);
            Append(c);
        }

        public LogicClauseBuilder(LogicClauseBuilder lcb)
        {
            _tokens = new(lcb._tokens);
            Tokens = new(_tokens);
            Arguments = lcb.Arguments;
        }

        public LogicClauseBuilder(IEnumerable<LogicToken> lts)
        {
            if (lts is ICollection<LogicToken> ic)
            {
                _tokens = new(ic.Count);
            }
            else
            {
                _tokens = new();
            }
            Tokens = new(_tokens);
            Append(lts);
        }

        public bool Complete => Arguments == 1;
        public int Arguments { get; private set; }


        public void OrWith(IEnumerable<LogicToken> ts)
        {
            Append(ts);
            Append(OperatorToken.AND);
        }

        public void OrWith(TermToken t)
        {
            if (Arguments < 1) throw new InvalidOperationException("Cannot apply binary operator to fewer than 2 arguments.");
            _tokens.Add(t);
            _tokens.Add(OperatorToken.OR);
        }

        public void OrWith(LogicClause c)
        {
            if (Arguments < 1) throw new InvalidOperationException("Cannot apply binary operator to fewer than 2 arguments.");
            for (int i = 0; i < c.Count; i++) _tokens.Add(c[i]);
            _tokens.Add(OperatorToken.OR);
        }

        public void OrWith(LogicClauseBuilder lcb)
        {
            if (Arguments + lcb.Arguments < 2) throw new InvalidOperationException("Cannot apply binary operator to fewer than 2 arguments.");
            Append(lcb);
            Append(OperatorToken.OR);
        }

        public void AndWith(IEnumerable<LogicToken> ts)
        {
            Append(ts);
            Append(OperatorToken.AND);
        }

        public void AndWith(TermToken t)
        {
            if (Arguments < 1) throw new InvalidOperationException("Cannot apply binary operator to fewer than 2 arguments.");
            _tokens.Add(t);
            _tokens.Add(OperatorToken.AND);
        }

        public void AndWith(LogicClause c)
        {
            if (Arguments < 1) throw new InvalidOperationException("Cannot apply binary operator to fewer than 2 arguments.");
            for (int i = 0; i < c.Count; i++) _tokens.Add(c[i]);
            _tokens.Add(OperatorToken.AND);
        }

        public void AndWith(LogicClauseBuilder lcb)
        {
            if (Arguments + lcb.Arguments < 2) throw new InvalidOperationException("Cannot apply binary operator to fewer than 2 arguments.");
            Append(lcb);
            Append(OperatorToken.AND);
        }

        public void Append(LogicToken lt)
        {
            if (lt is TermToken tt)
            {
                Append(tt);
            }
            else if (lt is OperatorToken ot)
            {
                Append(ot);
            }
            else throw new NotImplementedException("Unknown token " + lt);
        }

        public void Append(OperatorToken ot)
        {
            if (Arguments < 2) throw new InvalidOperationException("Cannot apply binary operator to fewer than 2 arguments.");
            else
            {
                _tokens.Add(ot);
                Arguments--;
            }
        }

        public void Append(TermToken tt)
        {
            _tokens.Add(tt);
            Arguments++;
        }

        public void Append(IEnumerable<LogicToken> lts)
        {
            foreach (LogicToken lt in lts) Append(lt);
        }

        public void Append(LogicClause c)
        {
            for (int i = 0; i < c.Count; i++) _tokens.Add(c[i]);
            Arguments++;
        }

        public void Append(LogicClauseBuilder lcb)
        {
            _tokens.AddRange(lcb._tokens);
            Arguments += lcb.Arguments;
        }

        public string ToInfix()
        {
            return Infix.ToInfix(_tokens);
        }
    }
}

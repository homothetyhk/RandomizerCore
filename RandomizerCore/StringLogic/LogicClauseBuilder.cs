using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace RandomizerCore.StringLogic
{
    public class LogicClauseBuilder
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

        /// <summary>
        /// Reduces ConstTokens from the expression.
        /// </summary>
        public void Simpl()
        {
            for (int i = 0; i < _tokens.Count - 1; i++)
            {
                if (_tokens[i] is ConstToken ct)
                {
                    RPN.GetOperationBoundToTerm(_tokens, i, out Range operandRange, out int opIndex);
                    OperatorToken op = (OperatorToken)_tokens[opIndex];
                    if (op.OperatorType == OperatorType.AND)
                    {
                        if (ct.Value)
                        {
                            _tokens.RemoveAt(opIndex);
                            _tokens.RemoveAt(i);
                            i--;
                        }
                        else
                        {
                            _tokens.RemoveAt(opIndex);
                            (int offset, int count) = operandRange.GetOffsetAndLength(_tokens.Count);
                            _tokens.RemoveRange(offset, count);
                            if (opIndex == i + 1) i -= count;
                            i--;
                        }
                    }
                    else
                    {
                        if (ct.Value)
                        {
                            _tokens.RemoveAt(opIndex);
                            (int offset, int count) = operandRange.GetOffsetAndLength(_tokens.Count);
                            _tokens.RemoveRange(offset, count);
                            if (opIndex == i + 1) i -= count;
                            i--;
                        }
                        else
                        {
                            _tokens.RemoveAt(opIndex);
                            _tokens.RemoveAt(i);
                            i--;
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Replaces MacroTokens with the clauses they represent. Acts recursively on the inserted clauses.
        /// </summary>
        public void Unfold()
        {
            for (int i = 0; i < _tokens.Count; i++)
            {
                if (_tokens[i] is MacroToken mt)
                {
                    _tokens.RemoveAt(i);
                    _tokens.InsertRange(i, mt.Source.GetMacro(mt.Name).Tokens);
                }
            }
        }

        /// <summary>
        /// Replaces all occurences that match the old token with the new token.
        /// </summary>
        public void Subst(TermToken oldToken, TermToken newToken)
        {
            for (int i = 0; i < _tokens.Count; i++)
            {
                if (_tokens[i] == oldToken) _tokens[i] = newToken;
            }
        }

        /// <summary>
        /// Replaces all occurences that match the old token with the new clause. Acts recursively.
        /// </summary>
        public void Subst(TermToken oldToken, LogicClause newClause)
        {
            for (int i = 0; i < _tokens.Count; i++)
            {
                if (_tokens[i] == oldToken)
                {
                    _tokens.RemoveAt(i);
                    _tokens.InsertRange(i, newClause.Tokens);
                }
            }
        }

        public string ToInfix()
        {
            return Infix.ToInfix(_tokens);
        }
    }
}

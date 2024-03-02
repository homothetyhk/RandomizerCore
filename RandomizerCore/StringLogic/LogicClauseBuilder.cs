using System.Collections.ObjectModel;

namespace RandomizerCore.StringLogic
{
    public class LogicClauseBuilder
    {
        private readonly List<LogicToken> _tokens;
        public readonly ReadOnlyCollection<LogicToken> Tokens;
        public bool Complete => Arguments == 1;
        public int Arguments { get; private set; }

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

        public LogicClauseBuilder(string infix, ITokenSource tokenSource) : this(Infix.Tokenize(infix, tokenSource)) { }

        public void OrWith(IEnumerable<LogicToken> ts)
        {
            Append(ts);
            Append(OperatorToken.OR);
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

        public void OrWith(string infix, ITokenSource tokenSource) => OrWith(Infix.Tokenize(infix, tokenSource));

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

        public void AndWith(string infix, ITokenSource tokenSource) => AndWith(Infix.Tokenize(infix, tokenSource));

        public void AndWithLeft(TermToken t)
        {
            if (!Complete) throw new InvalidOperationException("Prepend operations require a complete expression.");
            _tokens.Insert(0, t);
            _tokens.Add(OperatorToken.AND);
        }

        public void AndWithLeft(LogicClause c)
        {
            if (!Complete) throw new InvalidOperationException("Prepend operations require a complete expression.");
             _tokens.InsertRange(0, c);
            _tokens.Add(OperatorToken.AND);
        }

        public void AndWithLeft(LogicClauseBuilder lcb)
        {
            if (!Complete || !lcb.Complete) throw new InvalidOperationException("Prepend operations require a complete expression.");
            _tokens.InsertRange(0, lcb._tokens);
            _tokens.Add(OperatorToken.AND);
        }

        public void AndWithLeft(string infix, ITokenSource tokenSource) => AndWithLeft(new LogicClauseBuilder(infix, tokenSource));

        public void OrWithLeft(TermToken t)
        {
            if (!Complete) throw new InvalidOperationException("Prepend operations require a complete expression.");
            _tokens.Insert(0, t);
            _tokens.Add(OperatorToken.OR);
        }

        public void OrWithLeft(LogicClause c)
        {
            if (!Complete) throw new InvalidOperationException("Prepend operations require a complete expression.");
            _tokens.InsertRange(0, c);
            _tokens.Add(OperatorToken.OR);
        }

        public void OrWithLeft(LogicClauseBuilder lcb)
        {
            if (!Complete || !lcb.Complete) throw new InvalidOperationException("Prepend operations require a complete expression.");
            _tokens.InsertRange(0, lcb._tokens);
            _tokens.Add(OperatorToken.OR);
        }

        public void OrWithLeft(string infix, ITokenSource tokenSource) => OrWithLeft(new LogicClauseBuilder(infix, tokenSource));

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

        public void Append(string infix) => Append(Infix.Tokenize(infix));

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
                    _tokens.InsertRange(i, mt.Value.Tokens);
                    i--;
                }
            }
        }

        /// <summary>
        /// Replaces MacroTokens and ReferenceTokens with the clauses they represent. Acts recursively on the inserted clauses.
        /// </summary>
        public void Unfold(Func<string, LogicClause> referenceResolver)
        {
            for (int i = 0; i < _tokens.Count; i++)
            {
                if (_tokens[i] is MacroToken mt)
                {
                    _tokens.RemoveAt(i);
                    _tokens.InsertRange(i, mt.Value.Tokens);
                    i--;
                }
                else if (_tokens[i] is ReferenceToken rt)
                {
                    _tokens.RemoveAt(i);
                    _tokens.InsertRange(i, referenceResolver(rt.Target).Tokens);
                    i--;
                }
            }
        }

        /// <summary>
        /// Replaces CoalescingTokens with their result as determined by the delegate. Acts recursively on nested coalescing expressions.
        /// </summary>
        public void Coalesce(Func<TermToken, bool> tokenValidator)
        {
            for (int i = 0; i < _tokens.Count; i++)
            {
                if (_tokens[i] is CoalescingToken qt)
                {
                    _tokens[i] = tokenValidator(qt.Left) ? qt.Left : qt.Right;
                    i--;
                }
            }
        }

        /// <summary>
        /// Replaces CoalescingTokens with their result as determined by the delegate. If the delegate returns null, the CoalescingToken is left in place. 
        /// Acts recursively on nested coalescing expressions, provided the delegate returns nonnull.
        /// </summary>
        public void PartialCoalesce(Func<TermToken, bool?> tokenValidator)
        {
            for (int i = 0; i < _tokens.Count; i++)
            {
                if (_tokens[i] is CoalescingToken qt && tokenValidator(qt.Left) is bool b)
                {
                    _tokens[i] = b ? qt.Left : qt.Right;
                    i--;
                }
            }
        }

        /// <summary>
        /// Applies the delegate to each term in the expression. If the result of the delegate is not null, replaces the term at that position with the result.
        /// <br/>Returns the number of modified tokens.
        /// </summary>
        public int Transform(Func<TermToken, TermToken?> transformer)
        {
            int j = 0;
            for (int i = 0; i < _tokens.Count; i++)
            {
                if (_tokens[i] is TermToken orig && transformer(orig) is TermToken tt)
                {
                    _tokens[i] = tt;
                    j++;
                }
            }
            return j;
        }

        /// <summary>
        /// Distributes nested occurences of the inner operation acting on tokens of the type parameter.
        /// <br/>That is, if innerOp is OperatorToken.OR, then the result will decompose as a disjunction of clauses, such that within each clause, any disjunction does not contain any specified tokens.
        /// <br/>For example, if we push out token D, operator OR in "A | B + (C | D + (E | F) | G) | H", the result is "A | B + (C | G) | B + D + (E | F) | H".
        /// </summary>
        public void PushOut<T>(OperatorToken innerOp) where T : TermToken
        {
            OperatorToken outerOp = innerOp.OperatorType == OperatorType.OR ? OperatorToken.AND : OperatorToken.OR;

            for (int i = 0; i < _tokens.Count; i++)
            {
                if (_tokens[i] is T)
                {
                    int firstInner = -1;
                    int lastInner = -1;
                    int outer = -1;
                    for (int j = RPN.GetBoundOperatorAlt(_tokens, i); j != -1; j = RPN.GetBoundOperatorAlt(_tokens, j))
                    {
                        if (_tokens[j] == innerOp)
                        {
                            if (firstInner == -1) firstInner = j;
                            lastInner = j;
                        }
                        if (_tokens[j] == outerOp)
                        {
                            if (firstInner != -1)
                            {
                                outer = j;
                                break;
                            }
                        }
                    }
                    if (outer != -1)
                    {
                        if (firstInner != lastInner)
                        {
                            // the loop identifies an expression (a | (b | ... | SPECIALCONJ | ... | g)) + (...)
                            // we want to commute to (SPECIALCONJ | (a | ... | g)) + (...)
                            (int offset, int length) = RPN.GetEnclosingClause(_tokens, i, firstInner).GetOffsetAndLength(_tokens.Count);
                            _tokens.InsertRange(lastInner + 1, _tokens.GetRange(offset, length));
                            _tokens.Insert(lastInner + 1 + length, OperatorToken.OR);
                            _tokens.RemoveAt(firstInner);
                            _tokens.RemoveRange(offset, length);
                        }
                        // we now have (SPECIALCONJ | (a | ... | g)) + (...)
                        // we want to distribute to (SPECIALCONJ + ...) | ((a | ... | g) + (...))
                        if (lastInner == outer - 1)
                        {
                            (int innerRstart, int innerRcount) = RPN.GetClauseRangeFromEnd(_tokens, lastInner - 1).GetOffsetAndLength(_tokens.Count);
                            (int innerLstart, int innerLcount) = RPN.GetClauseRangeFromEnd(_tokens, innerRstart - 1).GetOffsetAndLength(_tokens.Count);
                            (int outerArgStart, int outerArgCount) = RPN.GetClauseRangeFromEnd(_tokens, innerLstart - 1).GetOffsetAndLength(_tokens.Count);

                            List<LogicToken> outerArgTokens = _tokens.GetRange(outerArgStart, outerArgCount);
                            _tokens.RemoveAt(outer);
                            _tokens.Insert(innerRstart + innerRcount, OperatorToken.AND);
                            _tokens.InsertRange(innerRstart, outerArgTokens);

                            _tokens.Insert(innerLstart + innerLcount, OperatorToken.AND);
                            _tokens.InsertRange(innerLstart, outerArgTokens);

                            _tokens.RemoveRange(outerArgStart, outerArgCount);
                        }
                        else
                        {
                            (int outerArgStart, int outerArgCount) = RPN.GetClauseRangeFromEnd(_tokens, outer - 1).GetOffsetAndLength(_tokens.Count);
                            (int innerRstart, int innerRcount) = RPN.GetClauseRangeFromEnd(_tokens, lastInner - 1).GetOffsetAndLength(_tokens.Count);
                            (int innerLstart, int innerLcount) = RPN.GetClauseRangeFromEnd(_tokens, innerRstart - 1).GetOffsetAndLength(_tokens.Count);

                            List<LogicToken> conjArgTokens = _tokens.GetRange(outerArgStart, outerArgCount);
                            _tokens.RemoveAt(outer);
                            _tokens.RemoveRange(outerArgStart, outerArgCount);

                            _tokens.Insert(innerRstart + innerRcount, OperatorToken.AND);
                            _tokens.InsertRange(innerRstart + innerRcount, conjArgTokens);

                            _tokens.Insert(innerLstart + innerLcount, OperatorToken.AND);
                            _tokens.InsertRange(innerLstart + innerLcount, conjArgTokens);
                        }
                        // inefficient, but easier than fixing the indices
                        i = -1; continue;
                    }
                }
            }
        }

        /// <summary>
        /// Distributes nested occurences of the inner operation acting on the specified tokens.
        /// <br/>That is, if innerOp is OperatorToken.OR, then the result will decompose as a disjunction of clauses, such that within each clause, any disjunction does not contain any specified tokens.
        /// <br/>For example, if we push out token D, operator OR in "A | B + (C | D + (E | F) | G) | H", the result is "A | B + (C | G) | B + D + (E | F) | H".
        /// </summary>
        public void PushOut(Func<TermToken, bool> predicate, OperatorToken innerOp)
        {
            OperatorToken outerOp = innerOp.OperatorType == OperatorType.OR ? OperatorToken.AND : OperatorToken.OR;

            for (int i = 0; i < _tokens.Count; i++)
            {
                if (_tokens[i] is TermToken tt && predicate(tt))
                {
                    int firstInner = -1;
                    int lastInner = -1;
                    int outer = -1;
                    for (int j = RPN.GetBoundOperatorAlt(_tokens, i); j != -1; j = RPN.GetBoundOperatorAlt(_tokens, j))
                    {
                        if (_tokens[j] == innerOp)
                        {
                            if (firstInner == -1) firstInner = j;
                            lastInner = j;
                        }
                        if (_tokens[j] == outerOp)
                        {
                            if (firstInner != -1)
                            {
                                outer = j;
                                break;
                            }
                        }
                    }
                    if (outer != -1)
                    {
                        if (firstInner != lastInner)
                        {
                            // the loop identifies an expression (a | (b | ... | SPECIALCONJ | ... | g)) + (...)
                            // we want to commute to (SPECIALCONJ | (a | ... | g)) + (...)
                            (int offset, int length) = RPN.GetEnclosingClause(_tokens, i, firstInner).GetOffsetAndLength(_tokens.Count);
                            _tokens.InsertRange(lastInner + 1, _tokens.GetRange(offset, length));
                            _tokens.Insert(lastInner + 1 + length, OperatorToken.OR);
                            _tokens.RemoveAt(firstInner);
                            _tokens.RemoveRange(offset, length);
                        }
                        // we now have (SPECIALCONJ | (a | ... | g)) + (...)
                        // we want to distribute to (SPECIALCONJ + ...) | ((a | ... | g) + (...))
                        if (lastInner == outer - 1)
                        {
                            (int innerRstart, int innerRcount) = RPN.GetClauseRangeFromEnd(_tokens, lastInner - 1).GetOffsetAndLength(_tokens.Count);
                            (int innerLstart, int innerLcount) = RPN.GetClauseRangeFromEnd(_tokens, innerRstart - 1).GetOffsetAndLength(_tokens.Count);
                            (int outerArgStart, int outerArgCount) = RPN.GetClauseRangeFromEnd(_tokens, innerLstart - 1).GetOffsetAndLength(_tokens.Count);

                            List<LogicToken> outerArgTokens = _tokens.GetRange(outerArgStart, outerArgCount);
                            _tokens.RemoveAt(outer);
                            _tokens.Insert(innerRstart + innerRcount, OperatorToken.AND);
                            _tokens.InsertRange(innerRstart, outerArgTokens);

                            _tokens.Insert(innerLstart + innerLcount, OperatorToken.AND);
                            _tokens.InsertRange(innerLstart, outerArgTokens);

                            _tokens.RemoveRange(outerArgStart, outerArgCount);
                        }
                        else
                        {
                            (int outerArgStart, int outerArgCount) = RPN.GetClauseRangeFromEnd(_tokens, outer - 1).GetOffsetAndLength(_tokens.Count);
                            (int innerRstart, int innerRcount) = RPN.GetClauseRangeFromEnd(_tokens, lastInner - 1).GetOffsetAndLength(_tokens.Count);
                            (int innerLstart, int innerLcount) = RPN.GetClauseRangeFromEnd(_tokens, innerRstart - 1).GetOffsetAndLength(_tokens.Count);

                            List<LogicToken> conjArgTokens = _tokens.GetRange(outerArgStart, outerArgCount);
                            _tokens.RemoveAt(outer);
                            _tokens.RemoveRange(outerArgStart, outerArgCount);

                            _tokens.Insert(innerRstart + innerRcount, OperatorToken.AND);
                            _tokens.InsertRange(innerRstart + innerRcount, conjArgTokens);

                            _tokens.Insert(innerLstart + innerLcount, OperatorToken.AND);
                            _tokens.InsertRange(innerLstart + innerLcount, conjArgTokens);
                        }
                        // inefficient, but easier than fixing the indices
                        i = -1; continue;
                    }
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
        /// Replaces all occurences that match the old token with the new clause. Is not recursive--it is safe to reference the old token inside the new clause.
        /// </summary>
        public void Subst(TermToken oldToken, LogicClause newClause)
        {
            for (int i = 0; i < _tokens.Count; i++)
            {
                if (_tokens[i] == oldToken)
                {
                    _tokens.RemoveAt(i);
                    _tokens.InsertRange(i, newClause.Tokens);
                    i += newClause.Count - 1;
                }
            }
        }

        public void Clear()
        {
            _tokens.Clear();
            Arguments = 0;
        }

        public string ToInfix()
        {
            return Infix.ToInfix(_tokens);
        }
    }
}

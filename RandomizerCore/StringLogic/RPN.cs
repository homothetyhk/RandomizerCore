using RandomizerCore.Logic;

namespace RandomizerCore.StringLogic
{
    /// <summary>
    /// Utility methods for dealing with logic in RPN form.
    /// </summary>
    public static class RPN
    {
        public static IEnumerable<LogicToken> OperateOver(IEnumerable<TermToken> terms, OperatorToken op)
        {
            var e = terms.GetEnumerator();
            if (!e.MoveNext()) yield break;
            yield return e.Current;
            while (e.MoveNext())
            {
                yield return e.Current;
                yield return op;
            }
        }

        public static IEnumerable<LogicToken> OperateOver(IEnumerable<IEnumerable<LogicToken>> clauses, OperatorToken op)
        {
            var e = clauses.GetEnumerator();
            if (!e.MoveNext()) yield break;
            foreach (var lt in e.Current) yield return lt;
            while (e.MoveNext())
            {
                foreach (var lt in e.Current) yield return lt;
                yield return op;
            }
        }


        /// <summary>
        /// Given the index of a term, finds the index of the operator it is bound to, and the range of indices corresponding to the other argument of the operator.
        /// </summary>
        internal static void GetOperationBoundToTerm(IReadOnlyList<LogicToken> logic, int termIndex, out Range operand, out int op)
        {
            if (logic == null) throw new ArgumentNullException(nameof(logic));
            if (termIndex < 0 || termIndex >= logic.Count) throw new ArgumentOutOfRangeException(nameof(termIndex));
            
            op = GetBoundOperator(logic, termIndex);

            if (op > termIndex + 1)
            {
                operand = new(termIndex + 1, op);
                return;
            }
            else
            {
                operand = GetClauseRangeFromEnd(logic, termIndex - 1);
                return;
            }
        }

        /// <summary>
        /// Given an index and a subsequent operator index, returns the range corresponding to the argument of the operator containing the first index.
        /// </summary>
        internal static Range GetEnclosingClause(IReadOnlyList<LogicToken> logic, int index, int op)
        {
            Range r = GetClauseRangeFromEnd(logic, op - 1);
            int boundary = r.Start.GetOffset(logic.Count);
            if (boundary <= index) return r;
            else return GetClauseRangeFromEnd(logic, boundary - 1);
        }

        /// <summary>
        /// Returns the index of the operator bound to the term or clause ending at startIndex.
        /// If startIndex points to the final token of the list, returns -1.
        /// </summary>
        public static int GetBoundOperatorAlt(IReadOnlyList<LogicToken> logic, int startIndex)
        {
            if (startIndex == logic.Count - 1) return -1;

            int operands = 1;
            for (int i = startIndex + 1; i < logic.Count; i++)
            {
                if (logic[i] is OperatorToken)
                {
                    operands--;
                    if (operands <= 1) return i;
                }
                else if (logic[i] is TermToken) operands++;
                else throw new ArgumentException("Unknown token found in logic.", nameof(logic));
            }
            throw new InvalidOperationException("Failed to find bound operator. Possibly malformed logic?");
        }

        /// <summary>
        /// Returns the index of the operator bound to the term at startIndex,
        /// </summary>
        public static int GetBoundOperator(IReadOnlyList<LogicToken> logic, int startIndex)
        {
            int operators = 0;
            int operands = 0;
            int index = startIndex;
            for (; index < logic.Count; index++)
            {
                if (logic[index] is OperatorToken) operators++;
                else if (logic[index] is TermToken) operands++;
                else throw new ArgumentException("Unknown token found in logic.", nameof(logic));

                if (operands - operators <= 1 && logic[index] is OperatorToken) break;
            }

            if (index == logic.Count) throw new InvalidOperationException("Malformed logic.");

            return index;
        }

        /// <summary>
        /// Returns the range of indices such that the corresponding tokens form a closed expression, ending at the specified index.
        /// <br/>If the token at the index is a term, the range will only contain that index.
        /// <br/>If the token is an operator, the range will contain the operator and its operands.
        /// </summary>
        internal static Range GetClauseRangeFromEnd(IReadOnlyList<LogicToken> logic, int inclusiveEndIndex)
        {
            int operators = 0;
            int operands = 0;
            int index = inclusiveEndIndex;
            for (; index >= 0; index--)
            {
                if (logic[index] is OperatorToken) operators++;
                else if (logic[index] is TermToken) operands++;
                else throw new ArgumentException("Unknown token found in logic.", nameof(logic));

                if (operands - operators == 1) break;
            }

            if (index == -1) throw new InvalidOperationException("Malformed logic.");

            return new(index, inclusiveEndIndex + 1);
        }

        /// <summary>
        /// Returns the disjunctive normal form of the expression. That is,
        /// <br/>ORing together the results of ANDing the terms in each list results in an expression equivalent to the input.
        /// </summary>
        public static List<HashSet<TermToken>> GetDNF(IReadOnlyList<LogicToken> logic)
        {
            Stack<List<HashSet<TermToken>>> stack = new();
            foreach (LogicToken lt in logic)
            {
                if (lt is OperatorToken ot)
                {
                    switch (ot.OperatorType)
                    {
                        case OperatorType.AND:
                            List<HashSet<TermToken>> andRight = stack.Pop();
                            List<HashSet<TermToken>> andLeft = stack.Pop();
                            List<HashSet<TermToken>> and = new(andRight.Count * andLeft.Count);
                            for (int i = 0; i < andRight.Count; i++)
                            {
                                for (int j = 0; j < andLeft.Count; j++)
                                {
                                    HashSet<TermToken> c = new(andRight[i].Count + andLeft[j].Count);
                                    c.UnionWith(andLeft[j]);
                                    c.UnionWith(andRight[i]);
                                    and.Add(c);
                                }
                            }
                            stack.Push(and);
                            break;
                        case OperatorType.OR:
                            List<HashSet<TermToken>> orRight = stack.Pop();
                            List<HashSet<TermToken>> orLeft = stack.Pop();
                            orLeft.AddRange(orRight);
                            stack.Push(orLeft);
                            break;
                    }
                }
                else if (lt is TermToken tt)
                {
                    stack.Push(new() { new() { tt } });
                }
                else throw new ArgumentException("Unknown token found in logic.");
            }

            if (stack.Count != 1) throw new ArgumentException("Logic was not a closed expression.");
            return stack.Pop();
        }
    }
}

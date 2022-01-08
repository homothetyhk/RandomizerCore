namespace RandomizerCore.StringLogic
{
    public static class RPN
    {
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

                if (operands - operators == 1 && logic[index] is OperatorToken) break;
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
        public static List<List<TermToken>> GetDNF(IReadOnlyList<LogicToken> logic)
        {
            Stack<List<List<TermToken>>> stack = new();
            foreach (LogicToken lt in logic)
            {
                if (lt is OperatorToken ot)
                {
                    switch (ot.OperatorType)
                    {
                        case OperatorType.AND:
                            List<List<TermToken>> andRight = stack.Pop();
                            List<List<TermToken>> andLeft = stack.Pop();
                            List<List<TermToken>> and = new(andRight.Count * andLeft.Count);
                            for (int i = 0; i < andRight.Count; i++)
                            {
                                for (int j = 0; j < andLeft.Count; j++)
                                {
                                    List<TermToken> c = new(andRight[i].Count + andLeft[j].Count);
                                    c.AddRange(andLeft[j]);
                                    c.AddRange(andRight[i]);
                                    and.Add(c);
                                }
                            }
                            stack.Push(and);
                            break;
                        case OperatorType.OR:
                            List<List<TermToken>> orRight = stack.Pop();
                            List<List<TermToken>> orLeft = stack.Pop();
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

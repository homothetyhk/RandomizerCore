namespace RandomizerCore.StringLogic
{
    /// <summary>
    /// Base class for an object which can evaluate tokenized logic.
    /// </summary>
    public abstract class StringPM
    {
        /// <summary>
        /// The logic processor and macro source tied to the PM.
        /// </summary>
        public readonly LogicProcessor LP = new();

        private readonly Dictionary<string, LogicClause> parsedLogic = new();
        private readonly Stack<bool> evalStack = new();

        public bool Evaluate(string infix)
        {
            if (!parsedLogic.TryGetValue(infix, out LogicClause clause))
            {
                parsedLogic.Add(infix, clause = LP.ParseInfixToClause(infix));
            }
            return Evaluate(clause);
        }

        public bool Evaluate(LogicClause clause)
        {
            for (int i = 0; i < clause.Count; i++)
            {
                if (clause[i] == OperatorToken.OR)
                {
                    bool right = evalStack.Pop();
                    bool left = evalStack.Pop();
                    evalStack.Push(left || right);
                }
                else if (clause[i] == OperatorToken.AND)
                {
                    bool right = evalStack.Pop();
                    bool left = evalStack.Pop();
                    evalStack.Push(left && right);
                }
                else
                {
                    evalStack.Push(Evaluate((TermToken)clause[i]));
                }
            }

            return evalStack.Pop();
        }

        public abstract bool Evaluate(TermToken token);
    }
}

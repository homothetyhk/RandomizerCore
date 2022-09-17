namespace RandomizerCore.Logic
{
    public static class LogicOperatorRef
    {
        private static readonly Dictionary<string, int> operators = new()
        {
            { "NONE", (int)LogicOperators.NONE },
            { "FALSE", (int)LogicOperators.NONE },
            { "ANY", (int)LogicOperators.ANY },
            { "TRUE", (int)LogicOperators.ANY },

            { "+", (int)LogicOperators.AND },
            { "|", (int)LogicOperators.OR },

            { ">", (int)LogicOperators.GT },
            { "<", (int)LogicOperators.LT },
            { "=", (int)LogicOperators.EQ },
            { "?", (int)LogicOperators.COALESCE },
        };

        public static bool TryGetOperator(string op, out int index)
        {
            return operators.TryGetValue(op, out index);
        }
    }
}

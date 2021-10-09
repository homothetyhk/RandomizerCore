using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Logic
{
    public enum LogicOperators
    {
        NONE = -1,
        ANY = -2,
        OR = -3,
        AND = -4,
        GT = -5,
        LT = -6,
        EQ = -7,
    }

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
        };

        public static bool TryGetOperator(string op, out int index)
        {
            return operators.TryGetValue(op, out index);
        }
    }
}

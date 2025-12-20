using RandomizerCore.StringParsing;

namespace RandomizerCore.StringLogic
{
    public class LogicOperatorProvider : IOperatorProvider
    {
        public const string AND = "+";
        public const string OR = "|";
        public const string GT = ">";
        public const string LT = "<";
        public const string EQ = "=";
        public const string PROJECT = "/";
        public const string REF = "*";
        public const string COALESCE = "?";

        private static readonly Dictionary<string, OperatorDefinition> operatorDefinitions = new()
        {
            [AND] = new(AND, 5, 4),
            [OR] = new(OR, 1, 0),
            [GT] = new(GT, 15, 14, "", ""),
            [LT] = new(LT, 15, 14, "", ""),
            [EQ] = new(EQ, 15, 14, "", ""),
            [PROJECT] = new(PROJECT, null, 20),
            [REF] = new(REF, 30, null),
            [COALESCE] = new(COALESCE, 11, 10),
        };

        public OperatorDefinition? GetDefinition(string op) => operatorDefinitions.TryGetValue(op, out OperatorDefinition def) ? def : null;

        public IReadOnlyCollection<string> GetAllOperators() => operatorDefinitions.Keys;

        /*
        public (int, int)? InfixBindingPower(string op) => op switch
        {
            OR => (0,1),
            AND => (4,5),
            COALESCE => (10,11),
            GT or LT or EQ => (14, 15),
            _ => null,
        };

        public int? PostfixBindingPower(string op) => op switch
        {
            PROJECT => 20,
            _ => null,
        };

        public int? PrefixBindingPower(string op) => op switch
        {
            REF => 30,
            _ => null,
        };
        */
    }
}

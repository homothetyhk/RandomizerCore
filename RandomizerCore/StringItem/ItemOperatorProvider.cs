using RandomizerCore.StringParsing;

namespace RandomizerCore.StringItem
{
    public class ItemOperatorProvider : IOperatorProvider
    {
        // infix operators
        // Term, Int -> Effect. Adds the provided value to the term
        public const string AdditionAssignment = "+=";
        // Term, Int -> Effect. Sets the term to the maximum of the provided value and its current value
        public const string MaxAssignment = "=/";
        // Bool, Effect -> Effect. If the condition is met, applies the effect
        public const string Conditional = "=>";
        // Effect, Effect -> Effect. Applies the right-hand effect if and only if the left-hand effect produces an empty item
        public const string ShortCircuitChaining = ">|>";
        // Effect, Effect -> Effect. Applies the 
        public const string Chaining = ">>";

        // prefix operators
        // where a bool is expected, the string is interpreted as the name of a logic def and its value is used as a bool.
        // where an item effect is expected, the string is interpreted as the name of an item and its effect is copied
        public const string Reference = "*";
        // Bool -> Bool. Negates the bool
        public const string Negation = "!";

        // postfix operators
        // if the associated term does not correspond to a term in the LogicManager, any effect that would be applied to that term is ignored
        public const string TermCoalescing = "?";
        // Term -> Effect. Adds 1 to the term.
        public const string Increment = "++";

        private static readonly IReadOnlyCollection<string> allOperators = new HashSet<string>()
        {
            AdditionAssignment,
            MaxAssignment,
            Conditional,
            ShortCircuitChaining,
            Chaining,
            Reference,
            Negation,
            TermCoalescing,
            Increment,
        };
        public IReadOnlyCollection<string> GetAllOperators() => allOperators;

        public int? PrefixBindingPower(string op) => op switch
        {
            Reference or Negation => 9,
            _ => null
        };

        public int? PostfixBindingPower(string op) => op switch
        {
            TermCoalescing or Increment => 11,
            _ => null
        };

        public (int, int)? InfixBindingPower(string op) => op switch
        {
            Chaining => (1, 2),
            ShortCircuitChaining => (3, 4),
            Conditional => (6, 5),
            MaxAssignment or AdditionAssignment => (7, 8),
            _ => null
        };
    }

}

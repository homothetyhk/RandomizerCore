using RandomizerCore.StringParsing;

namespace RandomizerCore.StringItems
{
    public class ItemOperatorProvider : IOperatorProvider
    {
        // infix operators
        /// <summary>
        /// Term, Int -> Effect. Adds the provided value to the term
        /// </summary>
        public const string AdditionAssignment = "+=";
        /// <summary>
        /// Term, Int -> Effect. Sets the term to the maximum of the provided value and its current value
        /// </summary>
        public const string MaxAssignment = "=/";
        /// <summary>
        /// Bool, Effect -> Effect. If the condition is met, applies the effect
        /// </summary>
        public const string Conditional = "=>";
        /// <summary>
        /// Effect, Effect -> Effect. Applies the right-hand effect if and only if the left-hand effect produces an empty item
        /// </summary>
        public const string ShortCircuitChaining = ">|>";
        /// <summary>
        /// Effect, Effect -> Effect. Applies the 
        /// </summary>
        public const string Chaining = ">>";

        // prefix operators
        /// <summary>
        /// Where a bool is expected, the string is interpreted as the name of a logic def and its value is used as a bool.
        /// Where an item effect is expected, the string is interpreted as the name of an item and its effect is copied
        /// </summary>
        public const string Reference = "*";
        /// <summary>
        /// Bool -> Bool. Negates the bool
        /// </summary>
        public const string Negation = "!"; // TODO

        // postfix operators
        /// <summary>
        /// If the associated term does not correspond to a term in the LogicManager, any effect that would be applied to that term is ignored
        /// </summary>
        public const string TermCoalescing = "?";
        /// <summary>
        /// Term -> Effect. Adds 1 to the term.
        /// </summary>
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

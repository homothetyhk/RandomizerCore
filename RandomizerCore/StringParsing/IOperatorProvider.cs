namespace RandomizerCore.StringParsing
{
    /// <summary>
    /// A record containing the syntax information of a domain-specific operator.
    /// </summary>
    /// <remarks>
    /// The "binding power" concept was borrowed from this explainer of Pratt Parsing:
    /// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html.
    /// It is essentially an alternative way to think about precedence, but gives a natural
    /// way to explain biased associativity.
    ///
    /// In short, each operator has a distinct left and right binding power. When looking at B
    /// in the expression `A &lt;op1&gt; B &lt;op2&gt; C`, you see whether the right BP of op1 is higher or
    /// lower than the left BP of op2 to see which operator has the stronger ability to "pull" or "bind"
    /// B into its expression. Left and right BP can be (and should be) asymmetric to give a well-defined
    /// preference for the case where op1 and op2 are the same operator (or 2 operators at the same precedence level).
    /// </remarks>
    /// <param name="Operator">The string representation of the operator.</param>
    /// <param name="PrefixBindingPower">The binding power of the operator toward the next expression (right binding power). Must be defined for prefix or infix operators, and must be null for postfix operators.</param>
    /// <param name="PostfixBindingPower">The binding power of the operator toward the previous expression (left binding power). Must be defined for postfix or infix operators, and must be null for prefix operators.</param>
    /// <param name="DefaultLeadingTrivia">The default whitespace to print before the operator.</param>
    /// <param name="DefaultTrailingTrivia">The default whitespace to print after the operator.</param>
    public record OperatorDefinition(string Operator, int? PrefixBindingPower, int? PostfixBindingPower, string DefaultLeadingTrivia, string DefaultTrailingTrivia)
    {
        public OperatorDefinition(string Operator, int? PrefixBindingPower, int? PostfixBindingPower) 
            : this(Operator, PrefixBindingPower, PostfixBindingPower,
                  PrefixBindingPower.HasValue && PostfixBindingPower.HasValue ? " " : "",
                  PrefixBindingPower.HasValue && PostfixBindingPower.HasValue ? " " : ""
                  ) { }

        public bool IsInfix => PrefixBindingPower.HasValue && PostfixBindingPower.HasValue;
        public bool IsPrefix => PrefixBindingPower.HasValue && !PostfixBindingPower.HasValue;
        public bool IsPostfix => PostfixBindingPower.HasValue && !PrefixBindingPower.HasValue;
    }


    /// <summary>
    /// Defines domain-specific operator definitions.
    /// </summary>
    /// <remarks>
    /// The "binding power" concept was borrowed from this explainer of Pratt Parsing:
    /// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html.
    /// It is essentially an alternative way to think about precedence, but gives a natural
    /// way to explain biased associativity.
    ///
    /// In short, each operator has a distinct left and right binding power. When looking at B
    /// in the expression `A &lt;op1&gt; B &lt;op2&gt; C`, you see whether the right BP of op1 is higher or
    /// lower than the left BP of op2 to see which operator has the stronger ability to "pull" or "bind"
    /// B into its expression. Left and right BP can be (and should be) asymmetric to give a well-defined
    /// preference for the case where op1 and op2 are the same operator (or 2 operators at the same precedence level).
    /// </remarks>
    public interface IOperatorProvider
    {
        /// <summary>
        /// Gets the definition of the operator, or null if the operator is not defined.
        /// </summary>
        OperatorDefinition? GetDefinition(string op);

        /// <summary>
        /// Get all operators defined by this provider.
        /// </summary>
        IReadOnlyCollection<string> GetAllOperators();

        /*
        /// <summary>
        /// Get the binding power of the operator if that operator is a known prefix operator, otherwise null
        /// </summary>
        int? PrefixBindingPower(string op);
        /// <summary>
        /// Get the binding power of the operator if that operator is a known postfix operator, otherwise null
        /// </summary>
        int? PostfixBindingPower(string op);
        /// <summary>
        /// Get the left and right binding powers of the operator if that operator is a known infix operator, otherwise null
        /// </summary>
        (int, int)? InfixBindingPower(string op);
        */
    }
}

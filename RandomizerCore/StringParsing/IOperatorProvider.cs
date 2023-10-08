namespace RandomizerCore.StringParsing
{
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
        /// Get all operators defined by this provider.
        /// </summary>
        IReadOnlyCollection<string> GetAllOperators();
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
    }
}

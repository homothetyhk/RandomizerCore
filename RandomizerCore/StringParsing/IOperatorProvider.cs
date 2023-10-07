namespace RandomizerCore.StringParsing
{
    // the "binding power" concept was borrowed from this explainer of pratt parsing:
    // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    // it is essentially an alternative way to think about precedence, but gives a natural
    // way to explain biased associativity
    //
    // TLDR; each operator has a distinct left and right binding power. when looking at B
    // in the expression `A <op1> B <op2> C`, you see whether the right bp of op1 is higher or
    // lower than the left bp of op2 to see which operator has the stronger ability to "pull" or "bind"
    // B into its expression. Left and right bp can be (and should be) asymmetric to give a well-defined
    // preference for the case where op1 and op2 are the same operator
    // (or 2 operators at the same precedence level)
    public interface IOperatorProvider
    {
        IReadOnlyCollection<string> GetAllOperators();
        int? PrefixBindingPower(string op);
        int? PostfixBindingPower(string op);
        (int, int)? InfixBindingPower(string op);
    }
}

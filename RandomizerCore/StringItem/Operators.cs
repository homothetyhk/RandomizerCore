using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.StringItem
{
    internal class OpTokenizerTree
    {
        private Dictionary<char, OpTokenizerTree> children = new();

        // this is mostly for debugging purposes, we don't actually need it but it's helpful
        // for path exploration
        public string? Value { get; private set; } = null;
        public ICollection<char> Candidates => children.Keys;

        public void Insert(string str)
        {
            char first = str[0];
            if (!children.TryGetValue(first, out OpTokenizerTree child))
            {
                child = new OpTokenizerTree();
                child.Value = (Value ?? "") + first;
                children[first] = child;
                // we don't need to go deeper
                if (str.Length == 1)
                {
                    return;
                }
            }
            else if (str.Length == 1)
            {
                // this is a case where we have a duplicate of the same operator, throw a fit
                // todo - better exception
                throw new Exception($"Duplicate operator defined: {(Value ?? "") + first}");
            }

            string rest = str[1..];
            child.Insert(rest);
        }

        public OpTokenizerTree Advance(char ch) => children[ch];
    }

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
    public static class Operators
    {
        // infix operators
        public const string AdditionAssignment = "+=";
        public const string MaxAssignment = "=/";
        public const string Conditional = "=>";
        public const string ShortCircuitChaining = ">|>";
        public const string Chaining = ">>";

        // prefix operators
        public const string Reference = "*";
        public const string Negation = "!";

        // postfix operators
        public const string TermCoalescing = "?";

        public static readonly IReadOnlyCollection<string> AllOperators = new HashSet<string>()
        {
            AdditionAssignment,
            MaxAssignment,
            Conditional,
            ShortCircuitChaining,
            Chaining,
            Reference,
            Negation,
            TermCoalescing
        };

        private static HashSet<char> reservedChars = new(AllOperators.SelectMany(x => x));
        public static bool IsReservedCharacter(char ch)
        {
            return reservedChars.Contains(ch);
        }

        public static int PrefixBindingPower(string op) => op switch
        {
            Reference or Negation => 9,
            _ => throw new NotImplementedException()
        };

        public static int PostfixBindingPower(string op) => op switch
        {
            TermCoalescing => 11,
            _ => throw new NotImplementedException()
        };

        public static (int, int) InfixBindingPower(string op) => op switch
        {
            Chaining => (1, 2),
            ShortCircuitChaining => (3, 4),
            Conditional => (5, 6),
            MaxAssignment or AdditionAssignment => (7, 8),
            _ => throw new NotImplementedException()
        };

        internal static readonly OpTokenizerTree TokenizerTree = new();
        static Operators()
        {
            foreach (string op in AllOperators)
            {
                TokenizerTree.Insert(op);
            }
        }
    }

}

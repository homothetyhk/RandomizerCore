using FluentAssertions;
using RandomizerCore.Logic;
using RandomizerCore.StringLogic;
using RandomizerCore.StringParsing;
using Xunit.Abstractions;

namespace RandomizerCoreTests.LogicExpressionTests
{
    public class ExtensionTests
    {
        public ITestOutputHelper Output { get; }
        public ExtensionTests(ITestOutputHelper Output)
        {
            this.Output = Output;
        }

        [Theory]
        [InlineData("A + B ? ORIG", "ORIG", true)]
        [InlineData("A + B", "ORIG", false)]
        [InlineData("A + (B | C)", "(B | C)", true)]
        [InlineData("A + (B | C)", "B|C", true)]
        [InlineData("*L + (B | C)", "L", true)]
        [InlineData("A > 2 + (B | C)", "2", true)]
        public void ContainsTest(string infix, string subinfix, bool expected)
        {
            Expression<LogicExpressionType> expr = LogicExpressionUtil.Parse(infix);
            Expression<LogicExpressionType> subexpr = LogicExpressionUtil.Parse(subinfix);
            expr.Contains(subexpr).Should().Be(expected);
        }

        [Theory]
        [InlineData("ORIG", "ORIG", "B", "B")]
        [InlineData("ORIG ? A", "ORIG", "B", "B ? A")]
        public void SubstTest(string infix, string toBeReplaced, string replacement, string expected)
        {
            Expression<LogicExpressionType> expr = LogicExpressionUtil.Parse(infix);
            Expression<LogicExpressionType> targetExpr = LogicExpressionUtil.Parse(toBeReplaced);
            Expression<LogicExpressionType> replacementExpr = LogicExpressionUtil.Parse(replacement);
            expr.Subst(targetExpr, replacementExpr, new LogicExpressionBuilder()).Print().Should().Be(expected);
        }

        [Theory]
        [InlineData("ORIG ? A", true, "ORIG")]
        [InlineData("ORIG ? A", false, "A")]
        [InlineData("A ? ORIG", true, "A ? ORIG")]
        [InlineData("A ? ORIG", false, "A ? ORIG")]
        [InlineData("A ? B", true, "A ? B")]
        [InlineData("A ? B", false, "A ? B")]
        public void PartialCoalesceTest(string infix, bool exists, string expected)
        {
            Expression<LogicExpressionType> expr = LogicExpressionUtil.Parse(infix);
            Expression<LogicExpressionType> origAtom = LogicExpressionUtil.Parse("ORIG");
            Expression<LogicExpressionType> result = expr.PartialCoalesce(e => e.IsEquivalentTo(origAtom) ? exists : null);
            result.Print().Should().Be(expected);
        }

        [Theory]
        [InlineData("*L + M", (string[])["M"], (string[])["A"], (string[])["L"], (string[])["T"], "T + A")]
        [InlineData("((*(L)) + (M))", (string[])["M"], (string[])["A"], (string[])["L"], (string[])["T"], "T + A")]
        public void UnfoldTests(string infix, string[] macroNames, string[] macroValues, string[] locationNames, string[] locationValues, string expected)
        {
            Expression<LogicExpressionType> expr = LogicExpressionUtil.Parse(infix);
            Expression<LogicExpressionType> result = expr.Unfold(
                s => Array.IndexOf(macroNames, s) is int i && i >= 0 ? LogicExpressionUtil.Parse(macroValues[i]) : null,
                s => Array.IndexOf(locationNames, s) is int i && i >= 0 ? LogicExpressionUtil.Parse(locationValues[i]) : null
                );
            result.Print().Should().Be(expected);
        }

        [Theory]
        [InlineData("A > 1", true)]
        [InlineData("B > 1", false)]
        [InlineData("B > 1 ? TRUE", true)]
        [InlineData("(B ? A) > 1", true)]
        public void IsDefinedTests(string input, bool expected)
        {
            LogicManagerBuilder lmb = new();
            lmb.GetOrAddTerm("A");
            LogicManager lm = new(lmb);
            Expression<LogicExpressionType> expr = LogicExpressionUtil.Parse(input);
            expr.IsDefined(lm).Should().Be(expected);
        }
    }
}

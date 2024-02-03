using FluentAssertions;
using RandomizerCore.Logic;
using RandomizerCore.StringItems;
using RandomizerCore.StringParsing;

namespace RandomizerCoreTests
{
    public class EffectToExpressionTests
    {
        [Theory]
        [InlineData("_")]
        [InlineData("A++")]
        [InlineData("A++ >> B++")]
        [InlineData("A++ >> B++ >> C++")]
        [InlineData("A++ >> B++ >|> C++")]
        [InlineData("`A<1` => B++")]
        [InlineData("!`A<1` => B++")]
        [InlineData("*I")]
        public void IdentityTest(string infix)
        {
            LogicManagerBuilder lmb = new();
            string[] terms = ["A", "B", "C"];
            foreach (string s in terms) lmb.GetOrAddTerm(s);
            lmb.AddItem(new StringItemTemplate("I", "_"));

            lmb.AddItem(new StringItemTemplate("Test_Item", infix));

            LogicManager lm = new(lmb);

            StringItem item = (StringItem)lm.GetItemStrict("Test_Item");

            item.Effect.ToEffectString().Should().Be(infix);
        }

        [Theory]
        [InlineData("A++ >> (B++ >> C++)", "A++ >> B++ >> C++")]
        [InlineData("A += 1", "A++")]
        [InlineData("A+=2", "A += 2")]
        public void NonidentityTest(string infix, string result)
        {
            LogicManagerBuilder lmb = new();
            string[] terms = ["A", "B", "C"];
            foreach (string s in terms) lmb.GetOrAddTerm(s);
            lmb.AddItem(new StringItemTemplate("I", "_"));

            lmb.AddItem(new StringItemTemplate("Test_Item", infix));

            LogicManager lm = new(lmb);

            StringItem item = (StringItem)lm.GetItemStrict("Test_Item");

            item.Effect.ToEffectString().Should().Be(result);
        }
    }
}

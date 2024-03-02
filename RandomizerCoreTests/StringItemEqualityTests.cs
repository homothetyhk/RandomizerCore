using FluentAssertions;
using RandomizerCore;
using RandomizerCore.Logic;
using RandomizerCore.StringItems;
namespace RandomizerCoreTests
{
    public class StringItemEqualityTests
    {
        [Theory]
        [InlineData("_ >|> _")]
        [InlineData("`A>1` => A++")]
        [InlineData("`A<1 | B<1 | A<2 + B<2` => (`A + B` => (A++ >> B++) >|> `A<1 + B>1` => A += 2 >|> B++)")]
        public void ItemEqualsTest(string effect)
        {
            LogicManagerBuilder lmb = new();
            lmb.GetOrAddTerm("A");
            lmb.GetOrAddTerm("B");

            LogicManager lm = new(lmb);

            LogicItem i1 = lm.FromItemString("I", effect);
            LogicItem i2 = lm.FromItemString("I", effect);

            i1.Should().Be(i2);
            i1.GetHashCode().Should().Be(i2.GetHashCode());
        }

    }
}

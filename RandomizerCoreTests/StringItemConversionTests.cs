using FluentAssertions;
using RandomizerCore;
using RandomizerCore.Logic;
using RandomizerCore.LogicItems;
using RandomizerCore.StringItems;
using RandomizerCore.StringParsing;

namespace RandomizerCoreTests
{
    public class StringItemConversionTests
    {
        [Fact]
        public void TestSingleItemConversion()
        {
            LogicManagerBuilder lmb = new();
            Term a = lmb.GetOrAddTerm("A");
            SingleItem item = new("I", new TermValue(a, 1));
            StringItemConversion.Convert(item).Print().Should().Be("A++");
            item = new("I", new TermValue(a, 2));
            StringItemConversion.Convert(item).Print().Should().Be("A += 2");
        }

        [Fact]
        public void TestMultiItemConversion()
        {
            LogicManagerBuilder lmb = new();
            Term a = lmb.GetOrAddTerm("A");
            MultiItem item = new("I", [new TermValue(a, 1), new TermValue(a, 2), new TermValue(a, 3)]);
            StringItemConversion.Convert(item).Print().Should().Be("A++ >> A += 2 >> A += 3");
        }

        [Fact]
        public void TestBoolItemConversion()
        {
            LogicManagerBuilder lmb = new();
            Term a = lmb.GetOrAddTerm("A");
            BoolItem item = new("I", a);
            StringItemConversion.Convert(item).Print().Should().Be("A =/ 1");
        }

        [Fact]
        public void TestCappedItemConversion()
        {
            LogicManagerBuilder lmb = new();
            Term a = lmb.GetOrAddTerm("A");
            CappedItem item = new("IF", [new TermValue(a, 1), new TermValue(a, 2), new TermValue(a, 3)], new TermValue(a, 5));
            StringItemConversion.Convert(item).Print().Should().Be("`A<5` => (A++ >> A += 2 >> A += 3)");
        }

        [Fact]
        public void TestBranchedItemConversion()
        {
            LogicManagerBuilder lmb = new();
            Term a = lmb.GetOrAddTerm("A");
            MultiItem trueItem = new("IT", [new TermValue(a, 1), new TermValue(a, 2), new TermValue(a, 3)]);
            CappedItem falseItem = new("IF", [new TermValue(a, 1), new TermValue(a, 2), new TermValue(a, 3)], new TermValue(a, 5));
            LogicManager lm = new(lmb);
            LogicDef ld = lm.FromString(new("L", "A=3 | A=4"));
            BranchedItem item = new("I", ld, trueItem, falseItem);
            StringItemConversion.Convert(item).Print().Should().Be("`A=3 | A=4` => (A++ >> A += 2 >> A += 3) >> !`A=3 | A=4` => `A<5` => (A++ >> A += 2 >> A += 3)");
            item = new("I", ld, trueItem, null);
            StringItemConversion.Convert(item).Print().Should().Be("`A=3 | A=4` => (A++ >> A += 2 >> A += 3)");
            item = new("I", ld, null, falseItem);
            StringItemConversion.Convert(item).Print().Should().Be("!`A=3 | A=4` => `A<5` => (A++ >> A += 2 >> A += 3)");
            item = new("I", ld, null, null);
            StringItemConversion.Convert(item).Print().Should().Be("`A=3 | A=4` => _");
        }
    }
}

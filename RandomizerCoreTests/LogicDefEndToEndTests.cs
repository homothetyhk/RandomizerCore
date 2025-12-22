using FluentAssertions;
using RandomizerCore;
using RandomizerCore.Exceptions;
using RandomizerCore.Logic;
using RandomizerCore.StringLogic;
using RandomizerCore.StringParsing;
using Xunit.Abstractions;

namespace RandomizerCoreTests
{
    public class LogicDefEndToEndTests
    {
        private ITestOutputHelper Output { get; }

        public LogicDefEndToEndTests(ITestOutputHelper Output)
        {
            this.Output = Output;
        }

        [Fact]
        public void WhiteSpaceWithinAtomTest()
        {
            new RandomizerCore.StringLogic.LogicClause(
"(Waterways_09[left1] | Waterways_09[right1]) + (LEFTCLAW | WINGS | (RIGHTCLAW + (LEFTDASH | LEFTSUPERDASH))) | $StartLocation[West Waterways]");
        }

        [Fact]
        public void TestLogicMacroCycleError()
        {
            LogicManagerBuilder lmb = new();
            lmb.AddMacro(new("M", "*L"));
            lmb.AddLogicDef(new("L", "M"));
            lmb.Invoking(lmb => new LogicManager(lmb)).Should().Throw<ArgumentException>().Which.GetBaseException().Should().BeOfType<ReferenceCycleException>();
        }

        [Fact]
        public void TestLogicSimple()
        {
            LogicManagerBuilder lmb = new();
            lmb.GetOrAddTerm("A");
            lmb.GetOrAddTerm("B");
            lmb.GetOrAddTerm("C");
            lmb.AddLogicDef(new("L1", "A + B + C"));
            lmb.AddLogicDef(new("L2", "A | B | C"));
            LogicManager lm = new(lmb);
            ProgressionManager pm = new(lm, null);
            Term a = lm.GetTermStrict("A");
            Term b = lm.GetTermStrict("B");
            Term c = lm.GetTermStrict("C");
            LogicDef l1 = lm.GetLogicDefStrict("L1");
            LogicDef l2 = lm.GetLogicDefStrict("L2");

            l1.ToInfix().Should().Be("A + B + C");
            l2.ToInfix().Should().Be("A | B | C");

            Term[] arr = [a, b, c];

            for (int flags = 0; flags < (1 << arr.Length); flags++)
            {
                bool conj = true;
                bool disj = false;
                for (int i = 0; i < arr.Length; i++)
                {
                    int bit = (flags >> i) & 1;
                    pm.Set(arr[i], bit);
                    conj &= bit > 0;
                    disj |= bit > 0;
                }
                l1.CanGet(pm).Should().Be(conj);
                l2.CanGet(pm).Should().Be(disj);
            }
        }

        [Fact]
        public void TestLogic1()
        {
            LogicManagerBuilder lmb = new();
            lmb.GetOrAddTerm("A");
            lmb.GetOrAddTerm("B");
            lmb.GetOrAddTerm("C");
            lmb.AddTransition(new("T1", "T1 | T2 + (A | B + C) | *L1 + B | NONE"));
            lmb.AddTransition(new("T2", "T2 | (T1 | *L1) + B"));
            lmb.AddTransition(new("T3", "ANY + NONE"));
            lmb.AddLogicDef(new("L1", "T1 + (A | B + C) | T2 + B")); // (T1 + (A | (B + C))) | (T2 + B)
            LogicManager lm = new(lmb);
            ProgressionManager pm = new(lm, null);
            Term a = lm.GetTermStrict("A");
            Term b = lm.GetTermStrict("B");
            Term c = lm.GetTermStrict("C");
            LogicTransition t1 = lm.GetTransitionStrict("T1");
            LogicTransition t2 = lm.GetTransitionStrict("T2");
            LogicTransition t3 = lm.GetTransitionStrict("T3");
            LogicDef l1 = lm.GetLogicDefStrict("L1");
            Output.WriteLine(((DNFLogicDef)l1).ToInfix());

            l1.CanGet(pm).Should().BeFalse();
            t1.CanGet(pm).Should().BeFalse();
            t2.CanGet(pm).Should().BeFalse();
            t3.CanGet(pm).Should().BeFalse();

            pm.Set(b, 1);
            pm.SetState(t2.term, lm.StateManager.DefaultStateSingleton);

            l1.CanGet(pm).Should().BeTrue();
            t1.CanGet(pm).Should().BeTrue();
            t2.CanGet(pm).Should().BeTrue();

            pm.SetState(t2.term, null);
            pm.SetState(t1.term, lm.StateManager.DefaultStateSingleton);

            l1.CanGet(pm).Should().BeFalse();
            t1.CanGet(pm).Should().BeTrue();
            t2.CanGet(pm).Should().BeTrue();

            pm.mu.AddTransitions(lm.TransitionLookup.Values);
            pm.mu.StartUpdating();

            l1.CanGet(pm).Should().BeTrue();
        }

        [Theory]
        [InlineData("ORIG ? A", "ORIG", true)]
        public void ContainsTest(string infix, string subInfix, bool expected)
        {
            Expression<LogicExpressionType> expr =  LogicExpressionUtil.Parse(infix);
            Expression<LogicExpressionType> sub = LogicExpressionUtil.Parse(subInfix);
            expr.Contains(sub).Should().Be(expected);
        }

        

        [Fact]
        public void OrigCoalesceTest()
        {
            LogicManagerBuilder lmb = new();
            lmb.GetOrAddTerm("A");
            lmb.DoLogicEdit(new("L1", "ORIG ? A"));
            lmb.LogicLookup["L1"].ToInfix().Should().Be("A", "the partial coalesce on a new logic definition eliminates ORIG.");
            lmb.DoLogicEdit(new("L1", "B ? ORIG"));
            lmb.LogicLookup["L1"].ToInfix().Should().Be("B ? A", "non-ORIG left-operands are not coalesced by the partial coalesce transformation.");
            lmb.DoLogicEdit(new("L1", "B"));
            lmb.DoLogicEdit(new("L1", "ORIG ? A"));
            lmb.LogicLookup["L1"].ToInfix().Should().Be("B", "the partial coalesce on an edit of existing logic preserves ORIG" +
                ", and the existing value is substituted for ORIG.");
            lmb.DoLogicEdit(new("L1", "ORIG ? ORIG"));
            lmb.LogicLookup["L1"].ToInfix().Should().Be("B");
        }

        [Theory]
        [InlineData("A > 5", (int[])[4,5,6], (bool[])[false, false, true])]
        [InlineData("A = 5", (int[])[4, 5, 6], (bool[])[false, true, false])]
        [InlineData("A < 5", (int[])[4, 5, 6], (bool[])[true, false, false])]
        public void ComparisonTest(string input, int[] values, bool[] expected)
        {
            LogicManagerBuilder lmb = new();
            Term a = lmb.GetOrAddTerm("A");
            lmb.AddLogicDef(new("L", input));
            LogicManager lm = new(lmb);
            LogicDef ld = lm.GetLogicDefStrict("L");
            ProgressionManager pm = new(lm, null);
            for (int i = 0; i < values.Length; i++)
            {
                pm.Set(a, values[i]);
                ld.CanGet(pm).Should().Be(expected[i]);
            }
        }

        [Obsolete]
        [Fact]
        public void ToTermTokenSequencesTest()
        {
            string[] tests = ["A=0", "A>0", "A<0", "A", "A>1", "T/", "*L", "*L/", "TRUE", "FALSE"];
            TermToken[] expected = [
                new ComparisonToken(ComparisonType.EQ, "A", "0"),
                new SimpleToken("A"),
                new ComparisonToken(ComparisonType.LT, "A", "0"),
                new SimpleToken("A"),
                new ComparisonToken(ComparisonType.GT, "A", "1"),
                new ProjectedToken(new SimpleToken("T")),
                new ReferenceToken("L"),
                new ProjectedToken(new ReferenceToken("L")),
                new ConstToken(true),
                new ConstToken(false),
                ];

            LogicManagerBuilder lmb = new();
            lmb.GetOrAddTerm("A");
            lmb.AddTransition(new RawLogicDef("T", "T"));
            lmb.AddLogicDef(new RawLogicDef("L", "T"));

            LogicManager lm = new(lmb);
            for (int i = 0; i < tests.Length; i++)
            {
                DNFLogicDef ld = lm.CreateDNFLogicDef(new("L" + i, tests[i]));
                ld.ToTermTokenSequences().Should().ContainSingle().Which
                    .Should().ContainSingle().Which
                    .Should().Be(expected[i], $"{tests[i]} is or simplifies to {expected[i].Write()}");
            }
        }
    }
}

using FluentAssertions;
using RandomizerCore;
using RandomizerCore.Exceptions;
using RandomizerCore.Logic;

namespace RandomizerCoreTests
{
    public class LogicDefEndToEndTests
    {
        [Fact]
        public void TestLogicMacroCycleError()
        {
            LogicManagerBuilder lmb = new();
            lmb.LP.SetMacro("M", "*L");
            lmb.AddLogicDef(new("L", "M"));
            lmb.Invoking(lmb => new LogicManager(lmb)).Should().Throw<ArgumentException>().Which.GetBaseException().Should().BeOfType<ReferenceCycleException>();
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
            lmb.AddLogicDef(new("L1", "T1 + (A | B + C) | T2 + B"));
            LogicManager lm = new(lmb);
            ProgressionManager pm = new(lm, null);
            Term a = lm.GetTermStrict("A");
            Term b = lm.GetTermStrict("B");
            Term c = lm.GetTermStrict("C");
            LogicTransition t1 = lm.GetTransitionStrict("T1");
            LogicTransition t2 = lm.GetTransitionStrict("T2");
            LogicTransition t3 = lm.GetTransitionStrict("T3");
            LogicDef l1 = lm.GetLogicDefStrict("L1");

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


    }
}

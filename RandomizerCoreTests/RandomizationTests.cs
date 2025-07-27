using FluentAssertions;
using RandomizerCore;
using RandomizerCore.Logic;
using RandomizerCore.Logic.StateLogic;
using RandomizerCore.Randomization;
using RandomizerCoreTests.Util;

namespace RandomizerCoreTests
{
    public class RandomizationTests
    {
        [Fact]
        public void IndeterminateLocationExpectedBehaviorTest()
        {
            LogicManagerBuilder lmb = new();
            lmb.AddWaypoint(new("Start", "$DEFAULTSTATE"));
            lmb.AddTransition(new("T1[left]", "T1[left] | Start"));
            lmb.AddTransition(new("T2[right]", "T2[right]"));
            lmb.AddTransition(new("T2[onewaysource]", "T2[right] + $I[SI1]"));
            lmb.AddTransition(new("T3[onewaytarget]", "NONE"));
            lmb.AddTransition(new("T3[bot]", "T3[bot] | T3[onewaytarget]"));
            lmb.AddTransition(new("T4[top]", "T4[top] | T4[right]"));
            lmb.AddTransition(new("T4[right]", "T4[right] | T4[top]"));
            lmb.AddTransition(new("T5[left]", "T5[left]"));
            lmb.VariableResolver = new TestVariableResolver(new RawStateData
            {
                Fields = new Dictionary<string, List<string>>
                {
                    { "Int", ["SI1"] }
                }
            });
            LogicManager lm = new(lmb);

            CoupledRandomizationGroup RightToLeft = new()
            {
                Items =
                [
                    new RandoTransition(lm.GetTransitionStrict("T1[left]")),
                    new RandoTransition(lm.GetTransitionStrict("T5[left]")),
                ],
                Locations =
                [
                    new RandoTransition(lm.GetTransitionStrict("T2[right]")),
                    new RandoTransition(lm.GetTransitionStrict("T4[right]")),
                ]
            };

            CoupledRandomizationGroup LeftToRight = new()
            {
                Items =
                [
                    new RandoTransition(lm.GetTransitionStrict("T2[right]")),
                    new RandoTransition(lm.GetTransitionStrict("T4[right]")),
                ],
                Locations =
                [
                    new RandoTransition(lm.GetTransitionStrict("T1[left]")),
                    new RandoTransition(lm.GetTransitionStrict("T5[left]")),
                ],
            };

            CoupledRandomizationGroup TopToBot = new()
            {
                Items =
                [
                    new RandoTransition(lm.GetTransitionStrict("T3[bot]")),
                ],
                Locations =
                [
                    new RandoTransition(lm.GetTransitionStrict("T4[top]"))
                ]
            };

            CoupledRandomizationGroup BotToTop = new()
            {
                Items =
                [
                    new RandoTransition(lm.GetTransitionStrict("T4[top]"))
                ],
                Locations =
                [
                    new RandoTransition(lm.GetTransitionStrict("T3[bot]")),
                ]
            };


            RightToLeft.Dual = LeftToRight;
            LeftToRight.Dual = RightToLeft;
            BotToTop.Dual = TopToBot;
            TopToBot.Dual = BotToTop;

            RandomizationGroup OneWay = new()
            {
                Items =
                [
                    new RandoTransition(lm.GetTransitionStrict("T3[onewaytarget]"))
                ],
                Locations =
                [
                    new RandoTransition(lm.GetTransitionStrict("T2[onewaysource]"))
                ]
            };

            ProgressionManager pm = new(lm, null);
            MainUpdater mu = pm.mu;
            mu.AddTransitions(lm.TransitionLookup.Values);
            mu.AddWaypoints(lm.Waypoints);
            foreach (RandomizationGroup g in new RandomizationGroup[] { RightToLeft, LeftToRight, BotToTop, TopToBot, OneWay })
            {
                pm.Add(g.Items);
                IndeterminateLocation il = new(lm, g);
                foreach (IRandoItem item in g.Items) if (item is ILocationDependentItem ildi) pm.AddLocationDependentEffect(ildi, il);
            }
            mu.StartUpdating();

            // the start state of T1[left] propagates to T2[right], T4[right], T5[left] through the indeterminate location
            // the state of T4[right] propagates to T4[top] through logic
            // the state of T4[top] propagates to T3[bot] through the indeterminate location
            // the state of T2[right] is modified and propagated to T2[onewaysource] through logic
            // the worse-than-start state of T2[onewaysource] is propagated to T3[onewaytarget] through the indeterminate location

            StateInt si1 = lm.StateManager.GetIntStrict("SI1");
            pm.GetState("T1[left]").Should().Satisfy(s => s.GetInt(si1) == 0);
            pm.GetState("T2[right]").Should().Satisfy(s => s.GetInt(si1) == 0);
            pm.GetState("T2[onewaysource]").Should().Satisfy(s => s.GetInt(si1) == 1);
            pm.GetState("T3[onewaytarget]").Should().Satisfy(s => s.GetInt(si1) == 1);
            pm.GetState("T3[bot]").Should().Satisfy(s => s.GetInt(si1) == 0);
            pm.GetState("T4[top]").Should().Satisfy(s => s.GetInt(si1) == 0);
            pm.GetState("T4[right]").Should().Satisfy(s => s.GetInt(si1) == 0);
            pm.GetState("T5[left]").Should().Satisfy(s => s.GetInt(si1) == 0);

            RightToLeft = new()
            {
                Items =
                [
                    new RandoTransition(lm.GetTransitionStrict("T1[left]")),
                ],
                Locations =
                [
                    new RandoTransition(lm.GetTransitionStrict("T2[right]")),
                ]
            };
            LeftToRight = new()
            {
                Items =
                [
                    new RandoTransition(lm.GetTransitionStrict("T2[right]")),
                ],
                Locations =
                [
                    new RandoTransition(lm.GetTransitionStrict("T1[left]")),
                ],
            };

            pm = new(lm, null);
            mu = pm.mu;
            mu.AddTransitions(lm.TransitionLookup.Values);
            mu.AddWaypoints(lm.Waypoints);
            foreach (RandomizationGroup g in new RandomizationGroup[] { RightToLeft, LeftToRight, BotToTop, TopToBot, OneWay })
            {
                pm.Add(g.Items);
                IndeterminateLocation il = new(lm, g);
                foreach (IRandoItem item in g.Items) if (item is ILocationDependentItem ildi) pm.AddLocationDependentEffect(ildi, il);
            }
            mu.StartUpdating();

            // the start state of T1[left] propagates to T2[right] through the indeterminate location
            // the state of T2[right] is modified and propagated to T2[onewaysource] through logic
            // the worse-than-start state of T2[onewaysource] is propagated to T3[onewaytarget] through the indeterminate location
            // the state of T3[onewaytarget] is propagated to T3[bot] through logic
            // the state of T3[bot] is propagated to T4[top] through the indeterminate location
            // the state of T4[top] is propagated to T4[right] through logic
            // since T4[right] and T5[left] are not randomized, they do not share state with T1[left] and T2[right]
            // the result is that everything downstream of T2[onewaysource] retains the worse state, and T5[left] never receives a state by any means

            pm.GetState("T1[left]").Should().Satisfy(s => s.GetInt(si1) == 0);
            pm.GetState("T2[right]").Should().Satisfy(s => s.GetInt(si1) == 0);
            pm.GetState("T2[onewaysource]").Should().Satisfy(s => s.GetInt(si1) == 1);
            pm.GetState("T3[onewaytarget]").Should().Satisfy(s => s.GetInt(si1) == 1);
            pm.GetState("T3[bot]").Should().Satisfy(s => s.GetInt(si1) == 1);
            pm.GetState("T4[top]").Should().Satisfy(s => s.GetInt(si1) == 1);
            pm.GetState("T4[right]").Should().Satisfy(s => s.GetInt(si1) == 1);
            pm.GetState("T5[left]").Should().BeNull();
        }



        // example where information is lost through indeterminate location
        // the transition counts imply that state must be modified to reach T3[right] from Start
        // however, indeterminate location is not sensitive to transition counts, so it allows the impossible T1[left] -> T3[right] improvement
        [Fact]
        public void IndeterminateLocationLossyBehaviorTest()
        {
            LogicManagerBuilder lmb = new();
            lmb.AddWaypoint(new("Start", "$DEFAULTSTATE"));
            lmb.AddTransition(new("T1[left]", "T1[left] | Start"));
            lmb.AddTransition(new("T2[right]", "T2[right] | T2[left] + $I[SI1]"));
            lmb.AddTransition(new("T2[left]", "T2[left] | T2[right] + $I[SI1]"));
            lmb.AddTransition(new("T3[right]", "T3[right]"));
            lmb.VariableResolver = new TestVariableResolver(new RawStateData
            {
                Fields = new Dictionary<string, List<string>>
                {
                    { "Int", ["SI1"] }
                }
            });
            LogicManager lm = new(lmb);
            StateInt si1 = lm.StateManager.GetIntStrict("SI1");

            CoupledRandomizationGroup RightToLeft = new()
            {
                Items =
                [
                    new RandoTransition(lm.GetTransitionStrict("T1[left]")),
                    new RandoTransition(lm.GetTransitionStrict("T2[left]")),
                ],
                Locations =
                [
                    new RandoTransition(lm.GetTransitionStrict("T2[right]")),
                    new RandoTransition(lm.GetTransitionStrict("T3[right]")),
                ]
            };

            CoupledRandomizationGroup LeftToRight = new()
            {
                Items =
                [
                    new RandoTransition(lm.GetTransitionStrict("T2[right]")),
                    new RandoTransition(lm.GetTransitionStrict("T3[right]")),
                ],
                Locations =
                [
                    new RandoTransition(lm.GetTransitionStrict("T1[left]")),
                    new RandoTransition(lm.GetTransitionStrict("T2[left]")),
                ],
            };
            RightToLeft.Dual = LeftToRight;
            LeftToRight.Dual = RightToLeft;

            ProgressionManager pm = new(lm, null);
            MainUpdater mu = pm.mu;
            mu.AddTransitions(lm.TransitionLookup.Values);
            mu.AddWaypoints(lm.Waypoints);
            foreach (RandomizationGroup g in new RandomizationGroup[] { RightToLeft, LeftToRight })
            {
                pm.Add(g.Items);
                IndeterminateLocation il = new(lm, g);
                foreach (IRandoItem item in g.Items) if (item is ILocationDependentItem ildi) pm.AddLocationDependentEffect(ildi, il);
            }
            mu.StartUpdating();

            lm.TransitionLookup.Values.Should().AllSatisfy(t => pm.GetState(t.term).Should().Satisfy(s => s.GetInt(si1) == 0));
        }


    }
}

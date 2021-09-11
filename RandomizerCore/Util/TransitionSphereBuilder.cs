using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;
using static RandomizerCore.LogHelper;

namespace RandomizerCore
{
    public class TransitionSphere
    {
        public int index;

        /// <summary>
        /// Transitions to be placed in the current sphere or earlier.
        /// </summary>
        public List<RandoTransition> placedTransitions;

        /// <summary>
        /// Transitions newly unlocked by the transitions placed in all earlier spheres.
        /// </summary>
        public List<RandoTransition> reachableTransitions;

        /// <summary>
        /// The count of reachable transitions, by direction, aggregated over current and earlier spheres, minus the corresponding counts of placed transitions.
        /// </summary>
        public DirectionCounts directionCounts;

        /// <summary>
        /// Selected progression values (e.g. geo, grubs, essence) tracked at each level. Keyed by lm index.
        /// </summary>
        public Dictionary<int, int> tracker = new();

        public string PrintOpenSphere()
        {
            StringBuilder sb = new();
            sb.AppendLine($"OPENED SPHERE {index}");
            sb.AppendLine($"REACHABLE: {string.Join(", ", reachableTransitions.Select(t => t.Name))}");
            sb.AppendLine(directionCounts.ToString());
            sb.AppendLine("======================");

            return sb.ToString();
        }

        public string PrintCloseSphere()
        {
            StringBuilder sb = new();
            sb.AppendLine($"CLOSED SPHERE {index}");
            sb.AppendLine($"PLACED: {string.Join(", ", placedTransitions.Select(t => t.Name))}");
            sb.AppendLine("TRACKER: " + string.Join(", ", tracker.Select(kvp => $"{kvp.Key}: {kvp.Value}")));
            sb.AppendLine("======================");

            return sb.ToString();
        }
    }

    public class TransitionSphereBuilder
    {
        readonly List<TransitionSphere> spheres = new List<TransitionSphere>();
        readonly TransitionInitializer ti;
        readonly int[] trackedTerms;
        readonly static string[] defaultTerms = new string[]
        {
            "GEO", "ESSENCE", "GRUBS"
        };

        public TransitionSphereBuilder(LogicManager lm, TransitionInitializer ti, string[] terms = null)
        {
            this.ti = ti;
            if (terms != null) trackedTerms = terms.Select(t => lm.GetTermIndex(t)).ToArray();
            else trackedTerms = defaultTerms.Select(t => lm.GetTermIndex(t)).ToArray();
        }

        public int SphereCount => spheres.Count;
        public TransitionSphere GetSphere(int i) => spheres[i];
        public TransitionSphere LastSphere => spheres[^1];
        public List<TransitionSphere> SphereList => spheres;


        public string PrintAll()
        {
            StringBuilder sb = new();

            for (int i = 0; i < SphereCount; i++)
            {
                sb.AppendLine(spheres[i].PrintOpenSphere());
                sb.AppendLine(spheres[i].PrintCloseSphere());
            }

            sb.AppendLine("Final balance:");
            sb.AppendLine(LastSphere.directionCounts.Step(Enumerable.Empty<RandoTransition>(), LastSphere.placedTransitions).ToString());

            return sb.ToString();
        }

        public void OpenSphere(List<RandoTransition> reachable)
        {
            TransitionSphere sphere = new TransitionSphere();
            sphere.index = spheres.Count;
            sphere.reachableTransitions = reachable;

            if (sphere.index == 0)
            {
                sphere.directionCounts = new DirectionCounts(ti);
                sphere.directionCounts.AddRange(reachable);
            }
            else sphere.directionCounts = LastSphere.directionCounts.Step(reachable, LastSphere.placedTransitions);
            
            spheres.Add(sphere);

            LogDebug(sphere.PrintOpenSphere());
        }

        public void CloseAndOpen(List<RandoTransition> placed, List<RandoTransition> reachable, ProgressionManager pm)
        {
            TransitionSphere last = spheres[^1];
            last.placedTransitions = placed;
            foreach (int i in trackedTerms) last.tracker[i] = pm.Get(i);
            foreach (var t in placed) if (t.placed != State.Permanent) throw new ArgumentException($"Placed was not set on transition {t.Name}");

            TransitionSphere next = new();
            next.index = spheres.Count;
            next.reachableTransitions = reachable;
            next.directionCounts = last.directionCounts.Step(reachable, placed);
            spheres.Add(next);
            foreach (var t in reachable) if (t.reachable != State.Permanent) throw new ArgumentException($"Reachable was not set on transition {t.Name}");

            LogDebug(last.PrintCloseSphere());
            LogDebug(next.PrintOpenSphere());
        }

        public void CloseSphere(List<RandoTransition> placed, ProgressionManager pm)
        {
            TransitionSphere sphere = spheres[^1];
            sphere.placedTransitions = placed;
            foreach (int i in trackedTerms) sphere.tracker[i] = pm.Get(i);

            LogDebug(sphere.PrintCloseSphere());
        }

        public void ValidateSphereCount()
        {
            DirectionCounts counts = new DirectionCounts(ti);
            counts.AddRange(spheres.SelectMany(s => s.placedTransitions.Concat(s.reachableTransitions)));
            Log(counts);
        }

        public void BuildSpheres(ProgressionManager pm, TransitionSelector selector, ReachableTransitions rt)
        {
            rt.Collect(out List<RandoTransition> newReachable);
            OpenSphere(newReachable);
            while (!selector.Finished)
            {
                Step(pm, selector, rt, LastSphere.directionCounts, out List<RandoTransition> placed);
                rt.Collect(out newReachable);
                if (!selector.Finished) CloseAndOpen(placed, newReachable, pm);
                else CloseSphere(placed, pm);
            }

            selector.CollectDiscardedTransitions(out List<RandoTransition> discarded);
            if (discarded.Count > 0 || rt.GetUnreachableSourceTransitions().Any())
            {
                // map unreachable transitions to useless transitions
                // this is a special case which is only relevant with decoupled transitions
                OpenSphere(rt.GetUnreachableSourceTransitions().ToList());
                CloseSphere(discarded, pm);
            }

            LogDebug(PrintAll());
        }

        /*
        // Old version of Step with defective Audit
        private static void Step(ProgressionManager pm, TransitionSelector selector, ReachableTransitions rt, DirectionCounts dc, out List<RandoTransition> placed)
        {
            RandoTransition auditResult = null;
            selector.SetDirectionCounts(dc);

            if (pm.Temp) throw new InvalidOperationException("Previous temp was not disposed!");
            pm.StartTemp();
            
            while (selector.TryProposeNext(out RandoTransition t))
            {
                t.Place(pm);
                if (rt.FoundNewTransitions) break;
                else if (Audit(t)) break;
            }

            if (!rt.FoundNewTransitions)
            {
                LogDebug("Finishing unsuccessful Step.");
                selector.Finish(out placed);
                pm.SaveTempItems();
                return;
            }
            else
            {
                LogDebug("Exited upward pass of Step successfully.");
                LogDebug(rt.PrintCurrent());
            }

            while (selector.TryRecallLast(out RandoTransition t))
            {
                if (t == auditResult) selector.RejectLast();
                else if (Decide(t) && IsFinalItem())
                {
                    break;
                }
                if (!rt.FoundNewTransitions) throw new Exception("Decide deleted necessary transition?!?!");
            }

            selector.FinishAccepting(out placed);
            pm.SaveTempItems();
            return;

            bool Audit(RandoTransition last)
            {
                rt.RemoveAuditTransition(last);
                int i = rt.AuditCount;
                if (i == 0) return false;
                LogDebug($"Beginning Audit with {i} transitions to check.");

                foreach (var audit in selector.GetProposedTransitions())
                {
                    if (rt.IsInAuditSet(audit))
                    {
                        LogDebug($"Auditting transition {audit.Name}!!!!!");
                        audit.placed = State.None;
                        pm.RestrictTempTo(selector.GetProposedTransitions().Where(t => t != audit));
                        if (rt.FoundNewTransitions)
                        {
                            LogDebug("Audit found new path!!!!!");
                            auditResult = audit;
                            return true;
                        }
                        else
                        {
                            LogDebug("Audit failed. Restoring original state...");
                            audit.Place(pm);
                            if (--i == 0) break;
                        }
                    }
                }

                if (i != 0) throw new InvalidOperationException("Audit not contained in placed transitions.");

                LogDebug("Exited audit.");
                rt.ClearAudit();
                return false;
            }

            bool Decide(RandoTransition t)
            {
                t.placed = State.None;
                //Log("Running Decide with transitions: " + string.Join(", ", selector.GetProposedItems().Skip(1).Concat(selector.GetAcceptedItems()).Select(t => t.Name)));
                pm.RestrictTempTo(selector.GetProposedItems().Skip(1).Concat(selector.GetAcceptedItems()));
                if (rt.FoundNewTransitions)
                {
                    selector.RejectLast(); // sets placed to none
                    //LogDebug(rt.PrintCurrent());
                    return false;
                }
                else
                {
                    t.Place(pm); // sets placed to temporary
                    selector.AcceptLast(); // sets placed to permanent
                    //LogDebug(rt.PrintCurrent());
                    return true;
                }
            }

            bool IsFinalItem()
            {
                //Log("Running IsFinalItem with transitions: " + string.Join(", ", selector.GetAcceptedItems().Select(t => t.Name)));
                pm.RestrictTempTo(selector.GetAcceptedItems());
                if (rt.FoundNewTransitions)
                {
                    //Log("IsFinalItem returns true.");
                    //LogDebug(rt.PrintCurrent());
                    return true;
                }
                else
                {
                    //Log("IsFinalItem returns false.");
                    //Log("Restoring " + string.Join(", ", selector.GetProposedItems().Select(t => t.Name)));
                    pm.Add(selector.GetProposedItems());
                    return false;
                }
            }
        }
        */


        private static void Step(ProgressionManager pm, TransitionSelector selector, ReachableTransitions rt, DirectionCounts dc, out List<RandoTransition> placed)
        {
            selector.SetDirectionCounts(dc);

            if (pm.Temp) throw new InvalidOperationException("Previous temp was not disposed!");
            pm.StartTemp();

            while (selector.TryProposeNext(out RandoTransition t))
            {
                t.Place(pm);
                if (rt.FoundNewTransitions) break;
            }

            if (!rt.FoundNewTransitions)
            {
                LogDebug("Auditing unsuccessful Step.");
                while (selector.TryRecallLast(out RandoTransition t))
                {
                    if (Audit(t))
                    {
                        selector.FinishAccepting(out placed);
                        pm.SaveTempItems();
                        return;
                    }
                }

                LogDebug("Finishing unsuccessful Step.");
                selector.UnacceptAll();
                selector.Finish(out placed);
                pm.SaveTempItems();
                return;
            }
            else
            {
                LogDebug("Exited upward pass of Step successfully.");
                LogDebug(rt.PrintCurrent());
            }

            while (selector.TryRecallLast(out RandoTransition t))
            {
                if (Decide(t) && IsFinalItem())
                {
                    break;
                }
                if (!rt.FoundNewTransitions) throw new Exception("Decide deleted necessary transition?!?!");
            }

            selector.FinishAccepting(out placed);
            pm.SaveTempItems();
            return;


            bool Decide(RandoTransition t)
            {
                t.placed = State.None;
                //Log("Running Decide with transitions: " + string.Join(", ", selector.GetProposedItems().Skip(1).Concat(selector.GetAcceptedItems()).Select(t => t.Name)));
                pm.RestrictTempTo(selector.GetProposedItems().Skip(1).Concat(selector.GetAcceptedItems()));
                if (rt.FoundNewTransitions)
                {
                    selector.RejectLast(); // sets placed to none
                    //LogDebug(rt.PrintCurrent());
                    return false;
                }
                else
                {
                    t.Place(pm); // sets placed to temporary
                    selector.AcceptLast(); // sets placed to permanent
                    //LogDebug(rt.PrintCurrent());
                    if (!rt.FoundNewTransitions) throw new InvalidOperationException("Lost new transitions during decide?!?!");
                    return true;
                }
            }

            bool AuditDecide(RandoTransition t, RandoTransition skip)
            {
                t.placed = State.None;
                LogDebug("Running AuditDecide with transitions: " + string.Join(", ", selector.GetProposedItems().Skip(1).Where(u => u != skip).Concat(selector.GetAcceptedItems()).Select(t => t.Name)));
                pm.RestrictTempTo(selector.GetProposedItems().Skip(1).Where(u => u != skip).Concat(selector.GetAcceptedItems()));
                if (rt.FoundNewTransitions)
                {
                    selector.RejectLast(); // sets placed to none
                    LogDebug(rt.PrintCurrent());
                    return false;
                }
                else
                {
                    t.Place(pm); // sets placed to temporary
                    selector.AcceptLast(); // sets placed to permanent
                    LogDebug(rt.PrintCurrent());
                    if (!rt.FoundNewTransitions) throw new InvalidOperationException("Lost new transitions during auditdecide?!?!");
                    return true;
                }
            }

            bool Audit(RandoTransition t)
            {
                if (!t.coupled)
                {
                    // a decoupled target does not block any source by being placed, so auditing it is pointless
                    selector.AcceptLast();
                    return false;
                }

                t.placed = State.None;
                LogDebug($"Running Audit on {t.Name} with transitions: " + string.Join(", ", selector.GetProposedItems().Skip(1).Concat(selector.GetAcceptedItems()).Select(t => t.Name)));
                pm.RestrictTempTo(selector.GetProposedItems().Skip(1).Concat(selector.GetAcceptedItems()));
                if (rt.FoundNewTransitions)
                {
                    LogDebug("Audit successful, resuming decision process...");
                    LogDebug(rt.PrintCurrent());
                    LogDebug(pm.ToString());


                    selector.UnacceptAll();
                    while (selector.TryRecallLast(out RandoTransition u) && u != t)
                    {
                        if (AuditDecide(u, t) && AuditIsFinalItem(t)) return true;
                    }

                    selector.RejectLast();
                    while (selector.TryRecallLast(out RandoTransition u) && u != t)
                    {
                        if (AuditDecide(u, t) && AuditIsFinalItem(t)) return true;
                    }

                    if (rt.FoundNewTransitions) return true;
                    else throw new InvalidOperationException("Lost new transition?!?!");
                }
                else
                {
                    LogDebug("Audit failed.");
                    t.Place(pm); // sets placed to temporary
                    selector.AcceptLast(); // sets placed to permanent
                    LogDebug(rt.PrintCurrent());
                    return false;
                }
            }

            bool IsFinalItem()
            {
                //Log("Running IsFinalItem with transitions: " + string.Join(", ", selector.GetAcceptedItems().Select(t => t.Name)));
                pm.RestrictTempTo(selector.GetAcceptedItems());
                if (rt.FoundNewTransitions)
                {
                    //Log("IsFinalItem returns true.");
                    LogDebug(rt.PrintCurrent());
                    return true;
                }
                else
                {
                    //Log("IsFinalItem returns false.");
                    //Log("Restoring " + string.Join(", ", selector.GetProposedItems().Select(t => t.Name)));
                    pm.Add(selector.GetProposedItems());
                    if (!rt.FoundNewTransitions) throw new InvalidOperationException("Lost new transitions during IsFinalItem?!?!");
                    return false;
                }
            }

            bool AuditIsFinalItem(RandoTransition skip)
            {
                //Log("Running IsFinalItem with transitions: " + string.Join(", ", selector.GetAcceptedItems().Select(t => t.Name)));
                pm.RestrictTempTo(selector.GetAcceptedItems());
                if (rt.FoundNewTransitions)
                {
                    //Log("IsFinalItem returns true.");
                    //LogDebug(rt.PrintCurrent());
                    return true;
                }
                else
                {
                    //Log("IsFinalItem returns false.");
                    //Log("Restoring " + string.Join(", ", selector.GetProposedItems().Select(t => t.Name)));
                    pm.Add(selector.GetProposedItems().Where(u => u != skip));
                    if (!rt.FoundNewTransitions) throw new InvalidOperationException("Lost new transitions during IsFinalItem?!?!");
                    return false;
                }
            }
        }


    }
}

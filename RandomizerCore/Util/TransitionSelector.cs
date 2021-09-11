using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Extensions;
using RandomizerCore.Logic;
using static RandomizerCore.LogHelper;

namespace RandomizerCore
{
    public class TransitionSelector
    {
        public bool Finished { get; private set; } = false;

        readonly Stack<RandoTransition> unusedItems;
        readonly Stack<RandoTransition> proposedItems;
        readonly Stack<RandoTransition> rejectedItems;
        List<RandoTransition> acceptedItems;
        List<RandoTransition> discardedTransitions;

        DirectionCounts dc;

        public TransitionSelector(IEnumerable<RandoTransition> ts)
        {
            unusedItems = new Stack<RandoTransition>(ts.Where(t => t.IsTargetTransition));
            proposedItems = new Stack<RandoTransition>(unusedItems.Count);
            rejectedItems = new Stack<RandoTransition>(unusedItems.Count);
            acceptedItems = new List<RandoTransition>();
            discardedTransitions = new();
        }


        public bool ShouldDiscard(RandoTransition t) => t.reachable == State.Permanent;
        public bool ShouldSkip(RandoTransition t) => !dc.HasMatch(t);
        public void SetDirectionCounts(DirectionCounts dc) => this.dc = dc;

        public IEnumerable<RandoTransition> GetAcceptedItems() => acceptedItems;
        public IEnumerable<RandoTransition> GetProposedItems() => proposedItems.Where(t => !ShouldSkip(t));
        public IEnumerable<RandoTransition> GetProposedTransitions() => proposedItems.Where(t => !ShouldSkip(t));

        private void Discard(RandoTransition t)
        {
            if (!t.coupled) discardedTransitions.Add(t);
        }

        public bool TryProposeNext(out RandoTransition t)
        {
            while (unusedItems.TryPop(out t))
            {
                if (ShouldDiscard(t))
                {
                    Discard(t);
                    continue;
                }
                else if (ShouldSkip(t))
                {
                    proposedItems.Push(t);
                    continue;
                }
                else
                {
                    t.placed = State.Temporary;
                    proposedItems.Push(t);
                    LogDebug($"Proposed {t.Name}");
                    return true;
                }
            }
            return false;
        }

        public bool TryRecallLast(out RandoTransition t)
        {
            while (proposedItems.TryPeek(out t))
            {
                if (ShouldSkip(t))
                {
                    rejectedItems.Push(proposedItems.Pop());
                }
                else
                {
                    LogDebug($"Recalled {t.Name}");
                    return true;
                }
            }
            return false;
        }

        public void AcceptLast()
        {
            while (proposedItems.TryPop(out RandoTransition t))
            {
                if (ShouldSkip(t)) rejectedItems.Push(t);
                else
                {
                    LogDebug($"Accepted {t.Name}");
                    t.placed = State.Permanent;
                    acceptedItems.Add(t);
                    return;
                }
            }
            throw new InvalidOperationException("AcceptLast called with no valid proposed transitions.");
        }

        public void UnacceptAll()
        {
            while (acceptedItems.Count > 0)
            {
                RandoTransition t = acceptedItems.Pop();
                t.placed = State.Temporary;
                proposedItems.Push(t);
            }
        }

        public void RejectLast()
        {
            while (proposedItems.TryPop(out RandoTransition t))
            {
                if (ShouldSkip(t)) rejectedItems.Push(t);
                else
                {
                    LogDebug($"Rejected {t.Name}");
                    t.placed = State.None;
                    rejectedItems.Push(t);
                    return;
                }
            }
            throw new InvalidOperationException("RejectLast called with no valid proposed transitions.");
        }

        /// <summary>
        /// Outputs list of accepted items. Moves all rejected items to proposed items and starts new acccepted item list.
        /// </summary>
        public void FinishAccepting(out List<RandoTransition> newItems)
        {
            newItems = acceptedItems;
            acceptedItems = new List<RandoTransition>();
            while (rejectedItems.TryPop(out RandoTransition item)) proposedItems.Push(item);
            while (proposedItems.TryPop(out RandoTransition item))
            {
                item.placed = State.None;
                unusedItems.Push(item);
            }
            if (unusedItems.Count == 0) Finished = true;
        }

        public void Finish(out List<RandoTransition> remainingItems)
        {
            if (acceptedItems.Count != 0) throw new InvalidOperationException("ItemSelector.Finish called with uncollected accepted items!");
            while (rejectedItems.TryPop(out RandoTransition r)) proposedItems.Push(r);
            while (proposedItems.TryPop(out RandoTransition p)) unusedItems.Push(p);
            remainingItems = new();
            while (unusedItems.TryPop(out RandoTransition t))
            {
                if (ShouldDiscard(t)) Discard(t);
                else remainingItems.Add(t);
            }

            foreach (var t in remainingItems) t.placed = State.Permanent;
            Finished = true;
        }

        /// <summary>
        /// Returns decoupled targets which are reachable but not placed.
        /// </summary>
        public void CollectDiscardedTransitions(out List<RandoTransition> discard)
        {
            discard = discardedTransitions;
            discardedTransitions = new();
        }
    }
}

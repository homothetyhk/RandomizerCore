using RandomizerCore.Collections;
using RandomizerCore.Exceptions;
using RandomizerCore.Extensions;

namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Selector which manages multiple GroupItemSelectors, with the goal of proposing lowest priority items from available RandomizationGroups.
    /// </summary>
    public class CombinedItemSelector
    {
        readonly GroupItemSelector[] selectors;

        readonly PriorityQueue<float, GroupItemSelector> openSelectors;
        readonly Stack<GroupItemSelector> proposeOrder;

        public CombinedItemSelector(RandomizationGroup[] groups)
        {
            selectors = new GroupItemSelector[groups.Length];
            for (int i = 0; i < selectors.Length; i++)
            {
                selectors[i] = new(groups[i]);
            }

            openSelectors = new();
            proposeOrder = new();
        }

        public bool Finished
        {
            get
            {
                return selectors.All(s => s.Finished);
            }
        }

        public IEnumerable<IRandoItem> GetAcceptedItems()
        {
            foreach (GroupItemSelector s in selectors)
            {
                foreach (IRandoItem r in s.GetAcceptedItems()) yield return r;
            }
        }

        public IEnumerable<IRandoItem> GetProposedItems()
        {
            foreach (GroupItemSelector s in selectors)
            {
                foreach (IRandoItem r in s.GetProposedItems()) yield return r;
            }
        }

        /// <summary>
        /// Enumerates all proposed or accepted items, except for the current proposed item.
        /// </summary>
        public IEnumerable<IRandoItem> GetTestItems()
        {
            GroupItemSelector s = proposeOrder.Peek();
            foreach (IRandoItem r in s.GetProposedItems().Skip(1))
            {
                yield return r;
            }
            foreach (IRandoItem r in s.GetAcceptedItems())
            {
                yield return r;
            }
            foreach (GroupItemSelector t in selectors)
            {
                if (t != s)
                {
                    foreach (IRandoItem r in t.GetProposedItems()) yield return r;
                    foreach (IRandoItem r in t.GetAcceptedItems()) yield return r;
                }
            }
        }

        public void AcceptLast()
        {
            proposeOrder.Pop().AcceptLast();
        }

        public void FinishAccepting(out List<IRandoItem>[] newItems)
        {
            newItems = new List<IRandoItem>[selectors.Length];
            for (int i = 0; i < newItems.Length; i++)
            {
                selectors[i].FinishAccepting(out newItems[i]);
            }
            proposeOrder.Clear();
        }

        public void RejectCurrentAndUnacceptAll()
        {
            GroupItemSelector s = proposeOrder.Peek();
            s.RejectCurrentAndUnacceptAll();
            foreach (GroupItemSelector t in selectors)
            {
                if (t != s) t.UnacceptAll();
            }
        }

        public void UnacceptAll()
        {
            foreach (GroupItemSelector s in selectors) s.UnacceptAll();
        }

        public void Finish(out List<IRandoItem>[] newItems)
        {
            newItems = new List<IRandoItem>[selectors.Length];
            for (int i = 0; i < newItems.Length; i++)
            {
                selectors[i].Finish(out newItems[i]);
            }
            proposeOrder.Clear();
        }

        public void RejectLast()
        {
            proposeOrder.Pop().RejectLast();
        }

        public bool TryProposeNext(out IRandoItem item)
        {
            if (!openSelectors.TryPeek(out _, out GroupItemSelector s))
            {
                item = default;
                return false;
            }
            
            if (!s.TryProposeNext(out item))
            {
                openSelectors.ExtractMin(); // this should not happen, now that trygetnextproposalpriority catches an empty selector
                return TryProposeNext(out item);
            }

            proposeOrder.Push(s);
            if (s.TryGetNextProposalPriority(out float priority))
            {
                openSelectors.UpdateHead(priority);
            }
            else
            {
                openSelectors.ExtractMin();
            }
            
            return true;
        }

        public bool TryRecallLast(out IRandoItem item, out bool coupled)
        {
            if (proposeOrder.TryPeek(out GroupItemSelector s))
            {
                coupled = s.coupled;
                return s.TryRecallLast(out item);
            }
            item = default;
            coupled = false;
            return false;
        }

        public bool TryRecallLast(out IRandoItem item)
        {
            if (proposeOrder.TryPeek(out GroupItemSelector s))
            {
                return s.TryRecallLast(out item);
            }
            item = default;
            return false;
        }

        public void UpdateCaps(Sphere[] last)
        {
            openSelectors.Clear();
            for (int i = 0; i < selectors.Length; i++)
            {
                selectors[i].IncrementCap(last[i].Locations.Count);
                if (selectors[i].TryGetNextProposalPriority(out float priority))
                {
                    openSelectors.Enqueue(priority, selectors[i]);
                }
            }
        }

        public void CollectDiscardedItems(out List<IRandoItem>[] discardedItems)
        {
            discardedItems = new List<IRandoItem>[selectors.Length];
            for (int i = 0; i < discardedItems.Length; i++)
            {
                selectors[i].CollectDiscardedTransitions(out discardedItems[i]);
            }
        }

    }
}

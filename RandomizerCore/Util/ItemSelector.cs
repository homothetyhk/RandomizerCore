using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Extensions;
using static RandomizerCore.LogHelper;

namespace RandomizerCore.Logic
{
    [Obsolete]
    public class ItemSelector
    {
        readonly Stack<RandoItem> unusedItems;
        readonly Stack<RandoItem> proposedItems;
        readonly Stack<RandoItem> rejectedItems;
        List<RandoItem> acceptedItems;
        public bool Finished { get; private set; } = false;

        public ItemSelector(IEnumerable<RandoItem> items)
        {
            unusedItems = new Stack<RandoItem>(items);
            proposedItems = new Stack<RandoItem>(unusedItems.Count);
            rejectedItems = new Stack<RandoItem>(unusedItems.Count);
            acceptedItems = new List<RandoItem>();
        }

        public bool TryProposeNext(out RandoItem item)
        {
            if (unusedItems.TryPop(out item))
            {
                LogDebug($"Proposed {item.Name}");
                proposedItems.Push(item);
                return true;
            }
            return false;
        }

        public bool TryRecallLast(out RandoItem item)
        {
            return proposedItems.TryPeek(out item);
        }

        public void AcceptLast()
        {
            var item = proposedItems.Pop();
            LogDebug($"Accepted {item.Name}");
            acceptedItems.Add(item);
        }

        public void RejectLast()
        {
            var item = proposedItems.Pop();
            LogDebug($"Rejected {item.Name}");
            rejectedItems.Push(item);
        }

        public IEnumerable<RandoItem> GetAcceptedItems() => acceptedItems;
        public IEnumerable<RandoItem> GetProposedItems() => proposedItems;
        public IEnumerable<RandoItem> GetRejectedItems() => rejectedItems;


        /// <summary>
        /// Outputs list of accepted items. Moves all rejected items to proposed items and starts new acccepted item list.
        /// </summary>
        public void FinishAccepting(out List<RandoItem> newItems)
        {
            newItems = acceptedItems;
            acceptedItems = new List<RandoItem>();
            while (rejectedItems.TryPop(out RandoItem item)) proposedItems.Push(item);
            if (proposedItems.Count == 0 && unusedItems.Count == 0) Finished = true;
        }

        public void Finish(out List<RandoItem> remainingItems)
        {
            if (acceptedItems.Count != 0) throw new InvalidOperationException("ItemSelector.Finish called with uncollected accepted items!");
            while (rejectedItems.TryPop(out RandoItem r)) proposedItems.Push(r);
            while (proposedItems.TryPop(out RandoItem p)) unusedItems.Push(p);
            remainingItems = new List<RandoItem>(unusedItems);
            Finished = true;
        }

        /// <returns>The collection of previously proposed items which have not yet been accepted.</returns>
        public IEnumerable<RandoItem> Resume()
        {
            return proposedItems;
        }

        public void Step(IEnumerable<RandoLocation> unreachableLocations, ProgressionManager pm, out List<RandoItem> newItems, out List<RandoLocation> newLocations)
        {
            ItemSelector IS = this;
            List<RandoLocation> rl = new List<RandoLocation>();

            bool TryFindNewReachableLocations()
            {
                rl.AddRange(unreachableLocations.Where(l => l.CanGet(pm)));
                return rl.Count != 0;
            }

            bool Decide(RandoItem item)
            {
                pm.RestrictTempTo(IS.GetProposedItems().Skip(1).Concat(IS.GetAcceptedItems()));
                if (rl.Any(l => l.CanGet(pm)))
                {
                    rl.RemoveAll(l => !l.CanGet(pm));
                    IS.RejectLast();
                    return false;
                }
                else
                {
                    IS.AcceptLast();
                    pm.Add(item);
                    return true;
                }

            }

            bool IsFinalItem()
            {
                pm.RestrictTempTo(IS.GetAcceptedItems());
                if (rl.Any(l => l.CanGet(pm)))
                {
                    rl.RemoveAll(l => !l.CanGet(pm));
                    return true;
                }
                else
                {
                    pm.Add(IS.GetProposedItems());
                    return false;
                }
            }

            pm.StartTemp();
            pm.Add(IS.Resume());
            if (!TryFindNewReachableLocations())
            {
                while (IS.TryProposeNext(out RandoItem item))
                {
                    pm.Add(item);
                    if (TryFindNewReachableLocations()) break;
                }

                if (rl.Count == 0)
                {
                    newItems = IS.GetProposedItems().ToList();
                    newLocations = rl;
                    pm.SaveTempItems();
                    return;
                }

                IS.AcceptLast();

                if (IsFinalItem())
                {
                    IS.FinishAccepting(out newItems);
                    newLocations = rl;
                    pm.SaveTempItems();
                    return;
                }
            }

            while (IS.TryRecallLast(out RandoItem item))
            {
                if (Decide(item) && IsFinalItem())
                {
                    break;
                }
            }

            IS.FinishAccepting(out newItems);
            newLocations = rl;
            pm.SaveTempItems();
        }
    }
}

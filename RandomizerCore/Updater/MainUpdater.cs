using RandomizerCore.Logic.StateLogic;
using RandomizerCore.Updater;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// Class which indexes subscribers by their dependency on progression terms to optimize updating in response to new progression.
    /// </summary>
    public class MainUpdater
    {
        public event Action OnReset;

        readonly TermIndexedCollection<List<UpdateEntry>> entriesByTerm;
        readonly List<UpdateEntry> individualEntries;
        readonly List<PMHook> pmHooks;

        readonly ProgressionManager pm;
        readonly HashSet<int> addEntryHelper;
        readonly HashQueue<int> updates;

        readonly RevertPoint longTermRevertPoint;
        readonly RevertPoint shortTermRevertPoint;

        bool active;

        private class RevertPoint
        {
            readonly TermIndexedCollection<int> revertCounts;
            int revertIndividualCount;
            int revertHookCount;

            public RevertPoint(TermCollection terms)
            {
                revertCounts = new(terms);
            }

            public void Set(MainUpdater mu)
            {
                if (mu.individualEntries.Count == revertIndividualCount && mu.pmHooks.Count == revertHookCount) return;
                revertIndividualCount = mu.individualEntries.Count;
                revertHookCount = mu.pmHooks.Count;
                revertCounts.PopulateFrom(mu.entriesByTerm, l => l.Count);
            }

            public void Apply(MainUpdater mu)
            {
                if (mu.individualEntries.Count != revertIndividualCount)
                {
                    mu.entriesByTerm.ZipAction(revertCounts, (i, l) => l.RemoveRange(i, l.Count - i));
                    mu.individualEntries.RemoveRange(revertIndividualCount, mu.individualEntries.Count - revertIndividualCount);
                }
                if (mu.pmHooks.Count != revertHookCount)
                {
                    if (mu.active) for (int i = revertHookCount; i < mu.pmHooks.Count; i++) mu.pmHooks[i].Unhook(mu.pm);
                    mu.pmHooks.RemoveRange(revertHookCount, mu.pmHooks.Count - revertHookCount);
                }
            }

        }

        public MainUpdater(LogicManager lm, ProgressionManager pm)
        {
            entriesByTerm = TermIndexedCollection<List<UpdateEntry>>.CreatePopulated<List<UpdateEntry>>(lm.Terms);
            individualEntries = new List<UpdateEntry>(2000);
            this.pm = pm;
            updates = new HashQueue<int>(lm.Terms.Count);
            addEntryHelper = new HashSet<int>(lm.Terms.Count);
            pmHooks = new();
            longTermRevertPoint = new(lm.Terms);
            shortTermRevertPoint = new(lm.Terms);
        }

        public void StartUpdating()
        {
            if (!active)
            {
                pm.AfterAddItem += EnqueueUpdates;
                pm.AfterAddRange += EnqueueUpdates;
                pm.AfterStartTemp += SetShortTermRevertPoint;
                pm.AfterEndTemp += OnEndTemp;
                foreach (PMHook hook in pmHooks) hook.Hook(pm);

                active = true;
            }

            DoUpdateAll();
        }

        

        public void StopUpdating()
        {
            if (active)
            {
                pm.AfterAddItem -= EnqueueUpdates;
                pm.AfterAddRange -= EnqueueUpdates;
                pm.AfterStartTemp -= SetShortTermRevertPoint;
                pm.AfterEndTemp -= OnEndTemp;
                foreach (PMHook hook in pmHooks) hook.Unhook(pm);
                active = false;
            }
        }

        private void SetShortTermRevertPoint()
        {
            shortTermRevertPoint.Set(this);
        }

        public void SetLongTermRevertPoint()
        {
            longTermRevertPoint.Set(this);
        }

        /// <summary>
        /// Resets the MU and rolls back entry list to the long term revert point.
        /// </summary>
        public void RevertLong()
        {
            Reset();
            longTermRevertPoint.Apply(this);
        }

        public void AddEntry(UpdateEntry entry)
        {
            foreach (Term term in entry.GetTerms())
            {
                if (addEntryHelper.Add(term.Id))
                {
                    entriesByTerm[term.Id].Add(entry);
                }
            }
            individualEntries.Add(entry);
            addEntryHelper.Clear();

            if (active) DoUpdateEntry(entry);
        }

        public void AddPMHook(PMHook hook)
        {
            pmHooks.Add(hook);
            if (active) hook.Hook(pm);
        }

        /// <summary>
        /// Adds entries with default waypoint behavior, inferring bool- or state-behavior based on term type.
        /// </summary>
        public void AddWaypoints(IEnumerable<LogicWaypoint> ps)
        {
            foreach (var p in ps)
            {
                if (p.term.Type == TermType.State)
                {
                    AddEntry(new StateUpdateEntry(p.term, p.logic));
                }
                else
                {
                    AddEntry(new PrePlacedItemUpdateEntry(p, p));
                }
            }
        }

        /// <summary>
        /// Adds entries which manage state for each transition.
        /// </summary>
        public void AddTransitions(IEnumerable<LogicTransition> ts)
        {
            foreach (var t in ts)
            {
                if (t.term.Type == TermType.State)
                {
                    AddEntry(new StateUpdateEntry(t.term, t.logic));
                }
            }
        }

        /// <summary>
        /// Adds entries which manage the state term according to its paired logic.
        /// <br/>Nonstate terms in the sequence are ignored.
        /// </summary>
        public void AddManagedStates(IEnumerable<(Term, StateLogicDef)> ts)
        {
            foreach (var t in ts)
            {
                if (t.Item1.Type == TermType.State) AddEntry(new StateUpdateEntry(t.Item1, t.Item2));
            }
        }

        public void LinkState(Term source, Term target)
        {
            AddEntry(new StateTransmittingUpdateEntry(source, target));
        }

        public void AddPlacements(IEnumerable<GeneralizedPlacement> ps)
        {
            foreach (var p in ps) AddEntry(new PrePlacedItemUpdateEntry(p.Item, p.Location));
        }

        public void AddPlacements(IEnumerable<RandoPlacement> ps)
        {
            foreach (var p in ps) AddEntry(new PrePlacedItemUpdateEntry(p.Item, p.Location));
        }

        public void AddEntries(IEnumerable<UpdateEntry> entries)
        {
            foreach (var entry in entries) AddEntry(entry);
        }

        public void EnqueueUpdates(ILogicItem item)
        {
            updates.Enqueue(item.GetAffectedTerms().Select(term => term.Id));
            DoUpdates();
        }

        public void EnqueueUpdates(IEnumerable<ILogicItem> items)
        {
            updates.Enqueue(items.SelectMany(item => item.GetAffectedTerms()).Select(t => t.Id));
            DoUpdates();
        }

        public void DoUpdates()
        {
            while (updates.TryDequeue(out int term)) DoUpdate(term);
        }

        public void DoUpdate(int term)
        {
            List<UpdateEntry> l = entriesByTerm[term];
            for (int i = 0; i < l.Count; i++)
            {
                DoUpdateEntry(l[i]);
            }
        }

        public void DoUpdateEntry(UpdateEntry entry)
        {
            if (entry.state == TempState.None && entry.CanGet(pm))
            {
                entry.state = pm.Temp ? TempState.Temporary : TempState.Permanent;
                entry.OnAdd(pm);
            }
            else if (entry.alwaysUpdate && entry.CanGet(pm))
            {
                entry.OnAdd(pm);
            }
        }

        public void DoUpdateAll()
        {
            for (int i = 0; i < individualEntries.Count; i++)
            {
                DoUpdateEntry(individualEntries[i]);
            }
        }

        public void OnRemove()
        {
            foreach (var entry in individualEntries)
            {
                if (entry.state == TempState.Temporary)
                {
                    entry.state = TempState.None;
                    entry.OnRemove(pm);
                }
            }
            shortTermRevertPoint.Apply(this);
        }

        public void OnEndTemp(bool saved)
        {
            if (!saved)
            {
                foreach (var entry in individualEntries)
                {
                    if (entry.state == TempState.Temporary)
                    {
                        entry.state = TempState.None;
                        entry.OnRemove(pm);
                    }
                }
                shortTermRevertPoint.Apply(this);
            }
            else
            {
                foreach (var entry in individualEntries)
                {
                    if (entry.state == TempState.Temporary)
                    {
                        entry.state = TempState.Permanent;
                    }
                }
            }
        }

        /// <summary>
        /// Resets each update entry in the updater.
        /// </summary>
        public void Reset()
        {
            StopUpdating();
            OnReset?.Invoke();
            foreach (UpdateEntry entry in individualEntries)
            {
                entry.Reset();
                entry.state = TempState.None;
            }
        }

        /// <summary>
        /// Removes all update entries from the updater, and clears its events.
        /// </summary>
        public void Clear()
        {
            individualEntries.Clear();
            foreach (List<UpdateEntry> list in entriesByTerm) list.Clear();
            OnReset = null;
        }

    }

    public abstract class UpdateEntry
    {
        public bool obtained => state != TempState.None;
        public TempState state;
        public virtual bool alwaysUpdate => false;
        public abstract bool CanGet(ProgressionManager pm);
        public abstract IEnumerable<Term> GetTerms();
        public abstract void OnAdd(ProgressionManager pm);
        public virtual void OnRemove(ProgressionManager pm) { }
        public virtual void Reset() { }
    }

    public abstract class PMHook
    {
        public abstract void Hook(ProgressionManager pm);
        public abstract void Unhook(ProgressionManager pm);
    }
}

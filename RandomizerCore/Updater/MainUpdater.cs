using RandomizerCore.Logic.StateLogic;
using RandomizerCore.Updater;
using System.Diagnostics;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// Class which indexes subscribers by their dependency on progression terms to optimize updating in response to new progression.
    /// </summary>
    public class MainUpdater
    {
        public event Action? OnReset;

        readonly TermIndexedCollection<List<UpdateEntryBase>> entriesByTerm;
        readonly List<UpdateEntryBase> individualEntries;
        readonly List<PMHook> pmHooks;

        readonly ProgressionManager pm;
        readonly HashSet<int> addEntryHelper;
        readonly HashQueue<int> updates;

        readonly RevertPoint longTermRevertPoint;
        readonly RevertPoint shortTermRevertPoint;

        public bool HasCustomLongTermRevertPoint { get; private set; }
        public object? Current { get; private set; }

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
                revertIndividualCount = mu.individualEntries.Count;
                revertHookCount = mu.pmHooks.Count;
                revertCounts.PopulateFrom(mu.entriesByTerm, l => l.Count);
            }

            public void Set(RevertPoint other)
            {
                revertIndividualCount = other.revertIndividualCount;
                revertHookCount = other.revertHookCount;
                revertCounts.PopulateFrom(other.revertCounts, i => i);
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
            entriesByTerm = TermIndexedCollection<List<UpdateEntryBase>>.CreatePopulated<List<UpdateEntryBase>>(lm.Terms);
            individualEntries = new List<UpdateEntryBase>(2000);
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

        public void LogAll()
        {
            Log("Logging mu entries...");
            foreach (UpdateEntryBase e in individualEntries) Log(e.ToString());
            Log("Finished logging mu entries");
        }

        private void SetShortTermRevertPoint()
        {
            shortTermRevertPoint.Set(this);
        }

        public void SetLongTermRevertPoint()
        {
            HasCustomLongTermRevertPoint = true;
            longTermRevertPoint.Set(this);
            shortTermRevertPoint.Set(longTermRevertPoint);
        }

        /// <summary>
        /// Resets the MU and rolls back entry list to the long term revert point.
        /// </summary>
        public void RevertLong()
        {
            Reset();
            shortTermRevertPoint.Set(longTermRevertPoint);
            longTermRevertPoint.Apply(this);
        }

        public void AddEntry(UpdateEntryBase entry)
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

            if (active) DoUpdate(entry);
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
            Stopwatch sw = Stopwatch.StartNew();
            while (updates.TryDequeue(out int term)) DoUpdate(term);
            sw.Stop();
            Profiling.EmitMetric("MainUpdater.DoUpdate.RuntimeUs", sw.Elapsed.TotalMilliseconds * 1000);
        }

        public void DoUpdate(int term)
        {
            List<UpdateEntryBase> l = entriesByTerm[term];
            for (int i = 0; i < l.Count; i++)
            {
                DoUpdate(l[i], term);
            }
        }

        public void DoUpdateAll()
        {
            for (int i = 0; i < individualEntries.Count; i++)
            {
                DoUpdate(individualEntries[i]);
            }
        }

        private void DoUpdate(UpdateEntryBase e)
        {
            Current = e;
            e.Update(pm);
            Current = null;
        }

        private void DoUpdate(UpdateEntryBase e, int term)
        {
            Current = e;
            e.Update(pm, term);
            Current = null;
        }

        public void OnEndTemp(bool saved)
        {
            foreach (var e in individualEntries) e.OnEndTemp(pm, saved);
            if (!saved) shortTermRevertPoint.Apply(this);
        }

        /// <summary>
        /// Resets each update entry in the updater.
        /// </summary>
        public void Reset()
        {
            StopUpdating();
            OnReset?.Invoke();
            foreach (UpdateEntryBase entry in individualEntries)
            {
                entry.Reset();
            }
        }

        /// <summary>
        /// Removes all update entries from the updater, and clears its events.
        /// </summary>
        public void Clear()
        {
            individualEntries.Clear();
            foreach (List<UpdateEntryBase> list in entriesByTerm) list.Clear();
            OnReset = null;
        }

    }

    public abstract class UpdateEntryBase
    {
        public abstract IEnumerable<Term> GetTerms();

        public abstract void Update(ProgressionManager pm, int updateTerm);
        public abstract void Update(ProgressionManager pm);
        public virtual void Reset() { }
        public virtual void OnEndTemp(ProgressionManager pm, bool saved) { }
    }

    public abstract class UpdateEntry : UpdateEntryBase
    {
        public bool obtained => state != TempState.None;
        public TempState state;
        public virtual bool alwaysUpdate => false;
        public override void Update(ProgressionManager pm)
        {
            if (!obtained && CanGet(pm))
            {
                state = pm.Temp ? TempState.Temporary : TempState.Permanent;
                OnAdd(pm);
            }
            else if (alwaysUpdate && CanGet(pm))
            {
                OnAdd(pm);
            }
        }
        public override void Update(ProgressionManager pm, int updateTerm)
        {
            if (!obtained && CanGet(pm))
            {
                state = pm.Temp ? TempState.Temporary : TempState.Permanent;
                OnAdd(pm);
            }
            else if (alwaysUpdate && CanGet(pm))
            {
                OnAdd(pm);
            }
        }
        public abstract bool CanGet(ProgressionManager pm);
        public abstract void OnAdd(ProgressionManager pm);
        public override void Reset()
        {
            state = TempState.None;
        }
        public override void OnEndTemp(ProgressionManager pm, bool saved)
        {
            if (state == TempState.Temporary)
            {
                if (!saved)
                {
                    state = TempState.None;
                    OnRemove(pm);
                }
                else
                {
                    state = TempState.Permanent;
                }
            }
        }

        public virtual void OnRemove(ProgressionManager pm) { }
    }

    public abstract class PMHook
    {
        public abstract void Hook(ProgressionManager pm);
        public abstract void Unhook(ProgressionManager pm);
    }
}

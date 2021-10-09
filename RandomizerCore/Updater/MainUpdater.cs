using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static RandomizerCore.LogHelper;

namespace RandomizerCore.Logic
{
    public class MainUpdater
    {
        readonly List<UpdateEntry>[] entriesByTerm;
        readonly List<UpdateEntry> individualEntries;
        readonly List<UpdateEntry> temp;
        ProgressionManager pm;
        readonly HashSet<int> addEntryHelper;
        readonly HashQueue<int> updates;

        public MainUpdater(LogicManager lm)
        {
            entriesByTerm = new List<UpdateEntry>[lm.TermCount];
            for (int i = 0; i < entriesByTerm.Length; i++) entriesByTerm[i] = new List<UpdateEntry>();
            individualEntries = new List<UpdateEntry>(2000);

            temp = new List<UpdateEntry>(128);
            updates = new HashQueue<int>(lm.TermCount);
            addEntryHelper = new HashSet<int>(lm.TermCount);
        }

        public void Hook(ProgressionManager pm)
        {
            this.pm = pm;
            pm.AfterAddItem += EnqueueUpdates;
            pm.AfterAddRange += EnqueueUpdates;
            pm.AfterEndTemp += OnEndTemp;
            pm.OnRemove += OnRemove;
            pm.AfterRemove += DoRecalculate;

            DoUpdateAll();
        }


        public event Action OnBeginRecalculate;
        public event Action OnEndRecalculuate;
        public event Action OnReset;

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

            if (pm != null) DoUpdateEntry(entry);
        }

        public void AddPlacements(IEnumerable<LogicWaypoint> ps)
        {
            foreach (var p in ps) AddEntry(new PrePlacedItemUpdateEntry(p, p));
        }

        public void AddEntries(IEnumerable<UpdateEntry> entries)
        {
            foreach (var entry in entries) AddEntry(entry);
        }

        public void AddPlacements(IEnumerable<ItemPlacement> ps)
        {
            foreach (var p in ps) AddEntry(new PrePlacedItemUpdateEntry(p));
        }

        public void AddPlacements(IEnumerable<TransitionPlacement> ps)
        {
            foreach (var p in ps) AddEntry(new TransitionUpdateEntry(p));
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
            //Log("Doing update for term: " + lm.GetTerm(term));
            foreach (var entry in entriesByTerm[term])
            {
                DoUpdateEntry(entry);
            }
        }

        public void DoUpdateEntry(UpdateEntry entry)
        {
            if (!entry.obtained && entry.CanGet(pm))
            {
                entry.obtained = true;
                if (pm.Temp)
                {
                    temp.Add(entry);
                }
                entry.OnAdd(pm);
            }
            else if (entry.alwaysUpdate && entry.CanGet(pm))
            {
                entry.OnAdd(pm);
            }
        }

        public void DoUpdateAll()
        {
            foreach (var entry in individualEntries)
            {
                DoUpdateEntry(entry);
            }
        }

        public void OnRemove()
        {
            //Log("Beginning OnRemove with " + temp.Count);

            for (int i = 0; i < temp.Count; i++)
            {
                var entry = temp[i];
                entry.obtained = false;
                entry.OnRemove(pm);
            }
        }

        public void DoRecalculate()
        {
            OnBeginRecalculate?.Invoke();

            bool updated;
            do
            {
                updated = false;
                for (int i = 0; i < temp.Count; i++)
                {
                    var entry = temp[i];
                    //Log(entry.ToString() + $" {entry.obtained}, {entry.CanGet(pm)}");
                    if (!entry.obtained && entry.CanGet(pm))
                    {
                        updated = true;
                        entry.obtained = true;
                        entry.OnAdd(pm);
                    }
                }
            }
            while (updated);

            //Log("Almost finished recalculate with " + temp.Count);
            temp.RemoveAll(e => !e.obtained);
            //Log("Finished recalculate with " + temp.Count);
            OnEndRecalculuate?.Invoke();
        }

        public void OnEndTemp(bool saved)
        {
            if (!saved)
            {
                for (int i = 0; i < temp.Count; i++)
                {
                    var entry = temp[i];
                    entry.obtained = false;
                    entry.OnRemove(pm);
                }
            }

            temp.Clear();
        }

        /// <summary>
        /// Resets each update entry in the updater.
        /// </summary>
        public void Reset()
        {
            pm = null;
            OnReset?.Invoke();
            foreach (UpdateEntry entry in individualEntries)
            {
                entry.Reset();
                entry.obtained = false;
            }
        }

        /// <summary>
        /// Removes all update entries from the updater, and clears its events.
        /// </summary>
        public void Clear()
        {
            individualEntries.Clear();
            foreach (List<UpdateEntry> list in entriesByTerm) list.Clear();
            temp.Clear();
            pm = null;
            OnBeginRecalculate = null;
            OnEndRecalculuate = null;
        }

    }

    public abstract class UpdateEntry
    {
        public bool obtained = false;
        public virtual bool alwaysUpdate => false;
        public abstract bool CanGet(ProgressionManager pm);
        public abstract IEnumerable<Term> GetTerms();
        public abstract void OnAdd(ProgressionManager pm);
        public abstract void OnRemove(ProgressionManager pm);

        public virtual void Reset() { }
    }
}

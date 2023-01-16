using RandomizerCore.Logic.StateLogic;
using RandomizerCore.StringLogic;
using System.Runtime.CompilerServices;

namespace RandomizerCore.Logic
{
    public class ProgressionManager
    {
        private readonly ProgressionData obtained;
        private readonly ProgressionData backup;


        public LogicManager lm { get; protected set; }
        public RandoContext? ctx { get; protected set; }
        public MainUpdater mu { get; protected set; }

        public bool Temp { get; private set; }

        public event Action<ILogicItem>? AfterAddItem;
        public event Action<IEnumerable<ILogicItem>>? AfterAddRange;
        public event Action? AfterStartTemp;
        public event Action<bool>? AfterEndTemp;


        public ProgressionManager(LogicManager lm, RandoContext? ctx)
        {
            this.lm = lm;
            this.ctx = ctx;
            this.mu = new(lm, this);

            obtained = new(lm);
            backup = new(lm);

            Reset();
        }

        public void Reset()
        {
            AfterAddItem = null;
            AfterAddRange = null;
            AfterStartTemp = null;
            AfterEndTemp = null;

            Temp = false;
            ProgressionData.Clear(obtained);
            ProgressionData.Clear(backup);
            mu.RevertLong();

            ctx?.InitialProgression?.AddTo(this);
        }


        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int Get(int id)
        {
            return obtained.GetValue(id);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public StateUnion? GetState(int id)
        {
            return obtained.GetState(id);
        }

        public StateUnion? GetState(string name)
        {
            return obtained.GetState(lm.GetTermStrict(name));
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SetState(int id, StateUnion? state)
        {
            obtained.SetState(id, state);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void GiveMinimumState(int id)
        {
            obtained.GiveMinimumState(id);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Set(int id, int value)
        {
            obtained.SetValue(id, value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Set(TermValue tv)
        {
            obtained.SetValue(tv.Term.Id, tv.Value);
        }

        public void Set(string term, int value)
        {
            obtained.SetValue(lm.GetTermStrict(term).Id, value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Incr(int id, int value)
        {
            obtained.Increment(id, value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Incr(TermValue tv)
        {
            obtained.Increment(tv.Term.Id, tv.Value);
        }

        public int Get(string id)
        {
            return obtained.GetValue(lm.GetTermStrict(id).Id);
        }

        public void Incr(string id, int incr)
        {
            obtained.Increment(lm.GetTermStrict(id).Id, incr);
        }

        /// <summary>
        /// Returns true if the value at the index is positive.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Has(int id)
        {
            return obtained.GetValue(id) > 0;
        }

        /// <summary>
        /// Returns true if the value at the index is greater than or equal to the threshold.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Has(int id, int threshold)
        {
            return obtained.GetValue(id) >= threshold;
        }

        /// <summary>
        /// Returns true if the value at the index is greater than or equal to the threshold.
        /// </summary>
        public bool Has(TermValue tv) => Has(tv.Term, tv.Value);

        /// <summary>
        /// Applies the effect of the item, then invokes AfterAddItem.
        /// </summary>
        public void Add(ILogicItem item)
        {
            item.AddTo(this);
            AfterAddItem?.Invoke(item);
        }

        /// <summary>
        /// Applies the effect of the item, and any location-dependent effects, then invokes AfterAddItem.
        /// </summary>
        public void Add(ILogicItem item, ILogicDef loc)
        {
            item.AddTo(this);
            if (item is ILocationDependentItem ildi) ildi.Place(this, loc);
            AfterAddItem?.Invoke(item);
        }

        /// <summary>
        /// Applies only the location-dependent effects of the item, then invokes AfterAddItem.
        /// </summary>
        public void AddLocationDependentEffect(ILocationDependentItem item, ILogicDef loc)
        {
            item.Place(this, loc);
            AfterAddItem?.Invoke(item);
        }

        /// <summary>
        /// Applies the effects of all items in the sequence, then invokes AfterAddRange.
        /// </summary>
        public void Add(IEnumerable<ILogicItem> items)
        {
            foreach (var item in items)
            {
                item.AddTo(this);
            }
            AfterAddRange?.Invoke(items);
        }

        public void StartTemp()
        {
            if (Temp) throw new InvalidOperationException("Previous Temp session was not terminated!");
            Temp = true;
            ProgressionData.Copy(obtained, backup);
            AfterStartTemp?.Invoke();
        }

        private void RestartTemp()
        {
            Temp = true;
            // the backup was just applied, so it is still correct
            AfterStartTemp?.Invoke();
        }

        public void RemoveTempItems()
        {
            if (!Temp) throw new InvalidOperationException("RemoveTempItems called outside of Temp!");
            Temp = false;
            ProgressionData.Copy(backup, obtained);
            AfterEndTemp?.Invoke(false);
        }

        public void RestrictTempTo(ILogicItem soleItem)
        {
            RemoveTempItems();
            RestartTemp();
            Add(soleItem);
        }

        public void RestrictTempTo(IEnumerable<ILogicItem> items)
        {
            RemoveTempItems();
            RestartTemp();
            Add(items);
        }


        public void SaveTempItems()
        {
            if (!Temp) throw new InvalidOperationException("SaveTempItems called outside of Temp!");
            Temp = false;
            AfterEndTemp?.Invoke(true);
        }

        public string Dump() => obtained.Dump(lm);
        public static string Diff(ProgressionManager left, ProgressionManager right) => ProgressionData.Diff(left.obtained, right.obtained);
        public static string Diff(ProgressionManager left, ProgressionData right) => ProgressionData.Diff(left.obtained, right);
        public static List<Term> GetDiffTerms(ProgressionManager left, ProgressionData right, ComparisonType type = ComparisonType.EQ) => ProgressionData.GetDiffTerms(left.obtained, right, type);

        public ProgressionData GetSnapshot() => obtained.DeepClone();
    }
}

using RandomizerCore.Logic.StateLogic;
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

        public event Action<ILogicItem> AfterAddItem;
        public event Action<IEnumerable<ILogicItem>> AfterAddRange;
        public event Action AfterStartTemp;
        public event Action<bool> AfterEndTemp;


        public ProgressionManager(LogicManager lm, RandoContext? ctx)
        {
            this.lm = lm;
            this.ctx = ctx;
            this.mu = new(lm, this);

            obtained = new(lm.Terms.Counts);
            backup = new(lm.Terms.Counts);

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
            return obtained.GetStateCollection(id);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public StateUnion? GetState(string name)
        {
            return obtained.GetStateCollection(lm.GetTerm(name));
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SetState(int id, StateUnion state)
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
            obtained.SetValue(lm.GetTerm(term).Id, value);
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
            return obtained.GetValue(lm.GetTerm(id).Id);
        }

        public void Incr(string id, int incr)
        {
            obtained.Increment(lm.GetTerm(id).Id, incr);
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

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Gt(int id, int threshold)
        {
            return obtained.GetValue(id) > threshold;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Lt(int id, int threshold)
        {
            return obtained.GetValue(id) < threshold;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Eq(int id, int threshold)
        {
            return obtained.GetValue(id) == threshold;
        }

        public void Add(ILogicItem item)
        {
            //if (Has(lm.GetTerm("King's_Brand")) && item.Name == "Grub") throw new Exception();
            //LogDebug("Add item " + item.Name);
            item.AddTo(this);
            AfterAddItem?.Invoke(item);
        }

        public void Add(IEnumerable<ILogicItem> items)
        {
            //LogDebug("Add items " + string.Join(", ", items.Select(i => i.Name)));
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
            StartTemp();
            Add(soleItem);
        }

        public void RestrictTempTo(IEnumerable<ILogicItem> items)
        {
            RemoveTempItems();
            StartTemp();
            Add(items);
        }


        public void SaveTempItems()
        {
            if (!Temp) throw new InvalidOperationException("SaveTempItems called outside of Temp!");
            Temp = false;
            AfterEndTemp?.Invoke(true);
        }

        public string Dump() => obtained.Dump(lm);
        public ProgressionData GetSnapshot() => obtained.DeepClone();
        /*
        /// <summary>
        /// Converts the ProgressionManager to a JSON-serialized dictionary with keys given by term names and values given by the integer values of the ProgressionManager for each term.
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new();
            sb.AppendLine("{");
            for (int i = 0; i < lm.TermCount; i++)
            {
                sb.AppendLine($"  \"{lm.GetTerm(i)}\": {obtained[i]},");
            }
            sb.AppendLine("}");
            return sb.ToString();
        }
        */
    }
}

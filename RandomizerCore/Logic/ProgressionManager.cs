using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace RandomizerCore.Logic
{
    public class ProgressionManager
    {
        private readonly int[] obtained;
        private readonly int[] backup;

        public LogicManager lm { get; protected set; }
        private readonly IRandomizerSettings rs;
        public RandoContext ctx { get; protected set; }

        public bool Temp { get; private set; }

        public event Action<ILogicItem> AfterAddItem;
        public event Action<IEnumerable<ILogicItem>> AfterAddRange;
        public event Action OnRemove;
        public event Action AfterRemove;
        public event Action AfterStartTemp;
        public event Action<bool> AfterEndTemp;


        public ProgressionManager(LogicManager lm, IRandomizerSettings gs, RandoContext ctx)
        {
            this.lm = lm;
            this.rs = gs;
            this.ctx = ctx;

            obtained = new int[lm.TermCount];
            backup = new int[lm.TermCount];

            Reset();
        }

        public void Reset()
        {
            AfterAddItem = null;
            AfterAddRange = null;
            OnRemove = null;
            AfterRemove = null;
            AfterStartTemp = null;
            AfterEndTemp = null;

            Temp = false;
            Array.Clear(obtained, 0, obtained.Length);
            Array.Clear(backup, 0, backup.Length);

            rs.ApplySettings(this);
        }


        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int Get(int index)
        {
            return obtained[index];
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Set(int index, int value)
        {
            obtained[index] = value;
        }

        public void Set(string term, int value)
        {
            obtained[lm.GetTerm(term).Id] = value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Incr(int index, int value)
        {
            obtained[index] += value;
        }


        public int Get(string id)
        {
            return obtained[lm.GetTerm(id).Id];
        }

        public void Incr(string id, int incr)
        {
            obtained[lm.GetTerm(id).Id] += incr;
        }

        /// <summary>
        /// /// Returns true if the value at the index is greater than or equal to 0.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Has(int index)
        {
            return obtained[index] > 0;
        }

        /// <summary>
        /// Returns true if the value at the index is greater than or equal to the threshold.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Has(int index, int threshold)
        {
            return obtained[index] >= threshold;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Gt(int index, int threshold)
        {
            return obtained[index] > threshold;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Lt(int index, int threshold)
        {
            return obtained[index] < threshold;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Eq(int index, int threshold)
        {
            return obtained[index] == threshold;
        }

        public void Add(ILogicItem item)
        {
            item.AddTo(this);
            AfterAddItem?.Invoke(item);
        }

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
            obtained.CopyTo(backup, 0);
            AfterStartTemp?.Invoke();
        }

        public void RemoveTempItems()
        {
            if (!Temp) throw new InvalidOperationException("RemoveTempItems called outside of Temp!");
            Temp = false;
            backup.CopyTo(obtained, 0);
            AfterEndTemp?.Invoke(false);
        }

        public void RestrictTempTo(ILogicItem soleItem)
        {
            if (!Temp) throw new InvalidOperationException("RestrictTempTo called outside of Temp!");
            backup.CopyTo(obtained, 0);
            soleItem.AddTo(this);
            OnRemove?.Invoke();
            AfterRemove?.Invoke();
        }

        public void RestrictTempTo(IEnumerable<ILogicItem> items)
        {
            if (!Temp) throw new InvalidOperationException("RestrictTempTo called outside of Temp!");
            backup.CopyTo(obtained, 0);

            foreach (var item in items)
            {
                item.AddTo(this);
            }
            
            OnRemove?.Invoke();
            AfterRemove?.Invoke();
        }


        public void SaveTempItems()
        {
            if (!Temp) throw new InvalidOperationException("SaveTempItems called outside of Temp!");
            Temp = false;
            AfterEndTemp?.Invoke(true);
        }

        public ProgressionSnapshot GetSnapshot()
        {
            return new(lm, obtained);
        }

        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("{");
            for (int i = 0; i < lm.TermCount; i++)
            {
                sb.AppendLine($"  {lm.GetTerm(i)}: {obtained[i]}");
            }
            sb.AppendLine("}");
            return sb.ToString();
        }
    }
}

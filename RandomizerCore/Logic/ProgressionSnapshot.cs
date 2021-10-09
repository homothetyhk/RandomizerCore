using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// Immutable progression manager.
    /// </summary>
    public class ProgressionSnapshot
    {
        private readonly int[] obtained;
        public LogicManager lm { get; }


        public ProgressionSnapshot(LogicManager lm, int[] obtained)
        {
            this.lm = lm;
            this.obtained = (int[])obtained.Clone();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int Get(int index)
        {
            return obtained[index];
        }

        public int Get(string id)
        {
            return obtained[lm.GetTerm(id).Id];
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

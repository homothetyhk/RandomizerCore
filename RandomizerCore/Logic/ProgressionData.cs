using RandomizerCore.Logic.StateLogic;
using RandomizerCore.StringLogic;
using System.Runtime.CompilerServices;
using System.Text;

namespace RandomizerCore.Logic
{
    public record ProgressionData(LogicManager LM, sbyte[] Data, int[] LargeData, StateUnion?[] StateData)
    {
        public ProgressionData(LogicManager LM) : this(LM,
            new sbyte[LM.Terms.GetTermCount(TermType.SignedByte)],
            new int[LM.Terms.GetTermCount(TermType.Int)],
            new StateUnion[LM.Terms.GetTermCount(TermType.State)]) { }

        public static void Clear(ProgressionData pd)
        {
            Array.Clear(pd.Data, 0, pd.Data.Length);
            Array.Clear(pd.LargeData, 0, pd.LargeData.Length);
            Array.Clear(pd.StateData, 0, pd.StateData.Length);
        }

        public static void Copy(ProgressionData from, ProgressionData to)
        {
            Array.Copy(from.Data, to.Data, from.Data.Length);
            Array.Copy(from.LargeData, to.LargeData, from.LargeData.Length);
            Array.Copy(from.StateData, to.StateData, from.StateData.Length);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int GetValue(int id)
        {
            return Term.GetTermType(id) switch
            {
                TermType.Int => LargeData[Term.GetIndex(id)],
                TermType.State => StateData[Term.GetIndex(id)] is not null ? 1 : 0,
                _ => Data[Term.GetIndex(id)],
            };
        }

        public void SetValue(int id, int value)
        {
            switch (Term.GetTermType(id))
            {
                case TermType.Int:
                    LargeData[Term.GetIndex(id)] = value;
                    break;
                case TermType.SignedByte:
                    if (value > sbyte.MaxValue) value = sbyte.MaxValue;
                    else if (value < sbyte.MinValue) value = sbyte.MinValue;
                    Data[Term.GetIndex(id)] = (sbyte)value;
                    break;
                default:
                    throw new InvalidCastException($"Term {LM.GetTerm(id).Name} of type {Term.GetTermType(id)} cannot be set to int value.");
            }
        }

        public void Increment(int id, int value)
        {
            int index = Term.GetIndex(id);
            switch (Term.GetTermType(id))
            {
                case TermType.Int:
                    int intValue = LargeData[index];
                    if (value > 0 && int.MaxValue - value < intValue) LargeData[index] = int.MaxValue;
                    else if (value < 0 && int.MinValue - value > intValue) LargeData[index] = int.MinValue;
                    else LargeData[index] = intValue + value;
                    break;
                case TermType.SignedByte:
                    int currentByte = Data[index];
                    if (value > 0 && sbyte.MaxValue - value < currentByte) Data[index] = sbyte.MaxValue;
                    if (value < 0 && sbyte.MinValue - value > currentByte) Data[index] = sbyte.MinValue;
                    else Data[index] = (sbyte)(currentByte + value);
                    break;
                default:
                    throw new InvalidCastException($"Term {LM.GetTerm(id).Name} of type {Term.GetTermType(id)} cannot be incremented by int value.");
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public StateUnion? GetState(int id)
        {
            return StateData[Term.GetIndex(id)];
        }

        public void SetState(int id, StateUnion? state)
        {
            if (Term.GetTermType(id) != TermType.State) throw new InvalidCastException($"Term {LM.GetTerm(id).Name} of type {Term.GetTermType(id)} cannot be set to state value.");
            StateData[Term.GetIndex(id)] = state;
        }

        public void GiveMinimumState(int id)
        {
            if (Term.GetTermType(id) != TermType.State) throw new InvalidCastException($"Term {LM.GetTerm(id).Name} of type {Term.GetTermType(id)} cannot be set to state value.");
            StateData[Term.GetIndex(id)] ??= StateUnion.Empty;
        }

        public string Dump(LogicManager lm)
        {
            StringBuilder sb = new StringBuilder('{').AppendLine();

            IReadOnlyList<Term> termList = lm.Terms.GetTermList(TermType.SignedByte);
            for (int i = 0; i < Data.Length; i++)
            {
                if (Data[i] != 0) sb.Append("  ").Append(termList[i].Name).Append(": ").Append(Data[i]).Append(',').AppendLine();
            }

            termList = lm.Terms.GetTermList(TermType.Int);
            for (int i = 0; i < LargeData.Length; i++)
            {
                if (LargeData[i] != 0) sb.Append("  ").Append(termList[i].Name).Append(": ").Append(LargeData[i]).Append(',').AppendLine();
            }

            termList = lm.Terms.GetTermList(TermType.State);
            for (int i = 0; i < StateData.Length; i++)
            {
                if (StateData[i] is not null) sb.Append("  ").Append(termList[i].Name).Append(": ").Append(lm.StateManager.PrettyPrint(StateData[i])).Append(',').AppendLine();
            }

            sb.Remove(sb.Length - Environment.NewLine.Length - 1, Environment.NewLine.Length + 1); // e.g. trailing ,\n or {\n
            return sb.Length != 0 ? sb.ToString() : "{}";
        }

        public static string Diff(ProgressionData left, ProgressionData right)
        {
            StringBuilder sb = new StringBuilder('{').AppendLine();
            LogicManager lm = left.LM;

            foreach (Term t in GetDiffTerms(left, right))
            {
                if (t.Type == TermType.State)
                {
                    sb.Append("  ").Append(t.Name).Append(": ").Append(lm.StateManager.PrettyPrint(left.GetState(t))).Append(" <> ").Append(lm.StateManager.PrettyPrint(right.GetState(t))).Append(',').AppendLine();
                }
                else
                {
                    sb.Append("  ").Append(t.Name).Append(": ").Append(left.GetValue(t)).Append(" <> ").Append(right.GetValue(t)).Append(',').AppendLine();
                }
            }

            sb.Remove(sb.Length - Environment.NewLine.Length - 1, Environment.NewLine.Length + 1); // e.g. trailing ,\n or {\n
            return sb.Length != 0 ? sb.ToString() : "{}";
        }

        public static List<Term> GetDiffTerms(ProgressionData left, ProgressionData right, ComparisonType type = ComparisonType.EQ)
        {
            return type switch
            {
                ComparisonType.GT => GetLT(right, left),
                ComparisonType.LT => GetLT(left, right),
                _ => GetNE(left, right),
            };
        }

        private static List<Term> GetLT(ProgressionData left, ProgressionData right)
        {
            List<Term> result = new();
            LogicManager lm = left.LM;

            IReadOnlyList<Term> termList = lm.Terms.GetTermList(TermType.SignedByte);
            for (int i = 0; i < left.Data.Length; i++)
            {
                if (left.Data[i] < right.Data[i]) result.Add(termList[i]);
            }

            termList = lm.Terms.GetTermList(TermType.Int);
            for (int i = 0; i < left.LargeData.Length; i++)
            {
                if (left.LargeData[i] < right.LargeData[i]) result.Add(termList[i]);
            }

            termList = lm.Terms.GetTermList(TermType.State);
            for (int i = 0; i < left.StateData.Length; i++)
            {
                if (StateUnion.IsProgressivelyLE(left.StateData[i], right.StateData[i])
                    && !StateUnion.IsProgressivelyLE(right.StateData[i], left.StateData[i])) result.Add(termList[i]);
            }

            return result;
        }

        private static List<Term> GetNE(ProgressionData left, ProgressionData right)
        {
            List<Term> result = new();
            LogicManager lm = left.LM;

            IReadOnlyList<Term> termList = lm.Terms.GetTermList(TermType.SignedByte);
            for (int i = 0; i < left.Data.Length; i++)
            {
                if (left.Data[i] != right.Data[i]) result.Add(termList[i]);
            }

            termList = lm.Terms.GetTermList(TermType.Int);
            for (int i = 0; i < left.LargeData.Length; i++)
            {
                if (left.LargeData[i] != right.LargeData[i]) result.Add(termList[i]);
            }

            termList = lm.Terms.GetTermList(TermType.State);
            for (int i = 0; i < left.StateData.Length; i++)
            {
                if (!StateUnion.IsProgressivelyEqual(left.StateData[i], right.StateData[i])) result.Add(termList[i]);
            }

            return result;
        }

        public ProgressionData DeepClone()
        {
            return new(LM, (sbyte[])Data.Clone(), (int[])LargeData.Clone(), (StateUnion?[])StateData.Clone());
        }

    }
}

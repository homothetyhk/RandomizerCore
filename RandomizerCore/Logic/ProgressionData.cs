using RandomizerCore.Json;
using RandomizerCore.Logic.StateLogic;
using System.Runtime.CompilerServices;

namespace RandomizerCore.Logic
{
    public readonly record struct ProgressionData(byte[] Data, int[] LargeData, StateUnion?[] StateData)
    {
        public ProgressionData(IReadOnlyList<int> counts) : this(new byte[counts[0]], new int[counts[1]], new StateUnion[counts[2]]) { }

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

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SetValue(int id, int value)
        {
            switch (Term.GetTermType(id))
            {
                case TermType.Int:
                    LargeData[Term.GetIndex(id)] = value;
                    break;
                case TermType.Byte:
                    if (value > byte.MaxValue) value = byte.MaxValue;
                    else if (value < byte.MinValue) value = byte.MinValue;
                    Data[Term.GetIndex(id)] = (byte)value;
                    break;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
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
                case TermType.Byte:
                    int currentByte = Data[index];
                    if (value > 0 && byte.MaxValue - value < currentByte) Data[index] = byte.MaxValue;
                    if (value < 0 && byte.MinValue - value > currentByte) Data[index] = byte.MinValue;
                    else Data[index] = (byte)(currentByte + value);
                    break;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public StateUnion? GetState(int id)
        {
            return StateData[Term.GetIndex(id)];
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SetState(int id, StateUnion state)
        {
            StateData[Term.GetIndex(id)] = state;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void GiveMinimumState(int id)
        {
            StateData[Term.GetIndex(id)] ??= StateUnion.Empty;
        }

        public string Dump(LogicManager lm)
        {
            Dictionary<string, object> o = new();

            IReadOnlyList<Term> termList = lm.Terms.GetTermList(TermType.Byte);
            for (int i = 0; i < Data.Length; i++)
            {
                if (Data[i] > 0) o.Add(termList[i].Name, (int)Data[i]);
            }

            termList = lm.Terms.GetTermList(TermType.Int);
            for (int i = 0; i < LargeData.Length; i++)
            {
                if (LargeData[i] != 0) o.Add(termList[i].Name, LargeData[i]);
            }

            termList = lm.Terms.GetTermList(TermType.State);
            for (int i = 0; i < StateData.Length; i++)
            {
                if (StateData[i] is not null) o.Add(termList[i].Name, lm.StateManager.PrettyPrint(StateData[i]));
            }

            return JsonUtil.Serialize(o);
        }

        public static string Diff(LogicManager lm, ProgressionData left, ProgressionData right)
        {
            Dictionary<string, object> o = new();

            IReadOnlyList<Term> termList = lm.Terms.GetTermList(TermType.Byte);
            for (int i = 0; i < left.Data.Length; i++)
            {
                int diff = right.Data[i] - left.Data[i];
                if (diff != 0) o.Add(termList[i].Name, diff);
            }

            termList = lm.Terms.GetTermList(TermType.Int);
            for (int i = 0; i < left.LargeData.Length; i++)
            {
                int diff = right.LargeData[i] - left.LargeData[i];
                if (diff != 0) o.Add(termList[i].Name, diff);
            }

            termList = lm.Terms.GetTermList(TermType.State);
            for (int i = 0; i < left.StateData.Length; i++)
            {
                string leftSt = lm.StateManager.PrettyPrint(left.StateData[i]);
                string rightSt = lm.StateManager.PrettyPrint(right.StateData[i]);
                if (leftSt != rightSt) o.Add(termList[i].Name, $"{leftSt} <> {rightSt}");
            }

            return JsonUtil.Serialize(o);
        }

        public ProgressionData DeepClone()
        {
            return new((byte[])Data.Clone(), (int[])LargeData.Clone(), (StateUnion?[])StateData.Clone());
        }

    }
}

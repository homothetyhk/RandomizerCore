using RandomizerCore.Json;
using RandomizerCore.Logic.StateLogic;
using System.Runtime.CompilerServices;

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
            Dictionary<string, object> o = new();

            IReadOnlyList<Term> termList = lm.Terms.GetTermList(TermType.SignedByte);
            for (int i = 0; i < Data.Length; i++)
            {
                if (Data[i] != 0) o.Add(termList[i].Name, (int)Data[i]);
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

        public static string Diff(ProgressionData left, ProgressionData right)
        {
            Dictionary<string, object> o = new();
            LogicManager lm = left.LM;

            IReadOnlyList<Term> termList = lm.Terms.GetTermList(TermType.SignedByte);
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
            return new(LM, (sbyte[])Data.Clone(), (int[])LargeData.Clone(), (StateUnion?[])StateData.Clone());
        }

    }
}

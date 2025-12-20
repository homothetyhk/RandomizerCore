using RandomizerCore.Collections;
using RandomizerCore.Exceptions;

namespace RandomizerCore.Logic
{
    internal class CycleDetector
    {
        private enum ArrowType
        {
            Macro,
            Reference,
            Item
        }

        private readonly record struct Entry(string Name, ArrowType Type);


        private readonly HashStack<Entry> entries = [];
        private readonly List<Entry> record = [];

        public void PushMacro(string macro)
        {
            Entry e = new(macro, ArrowType.Macro);
            record.Add(e);
            if (!entries.Push(e)) throw CycleError(e);
        }

        public void PushReference(string reference)
        {
            Entry e = new(reference, ArrowType.Reference);
            record.Add(e);
            if (!entries.Push(e)) throw CycleError(e);
        }

        public void PushItem(string item)
        {
            Entry e = new(item, ArrowType.Reference);
            record.Add(e);
            if (!entries.Push(e)) throw CycleError(e);
        }

        public void Pop()
        {
            entries.Pop();
        }

        public void Clear() => entries.Clear();

        private Exception CycleError(Entry e)
        {
            string msg = $"Reference cycle detected: {e} <- \n{string.Join(" <- ", entries)}\n{string.Join(", ", record)}";
            return new ReferenceCycleException(msg);
        }

    }
}
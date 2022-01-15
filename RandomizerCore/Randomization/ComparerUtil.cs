using RandomizerCore.Collections;

namespace RandomizerCore.Randomization
{
    public static class ComparerUtil
    {
        public static IEqualityComparer<IRandoLocation> LocationEqualityComparer { get; } = ReferenceEqualityComparer<IRandoLocation>.Instance;
        public static IEqualityComparer<IRandoItem> ItemEqualityComparer { get; } = ReferenceEqualityComparer<IRandoItem>.Instance;
        public static IComparer<IRandoLocation> LocationComparer { get; } = new RandoLocationComparer();
        public static IComparer<IRandoItem> ItemComparer { get; } = new RandoItemComparer();

        private class RandoItemComparer : Comparer<IRandoItem>
        {
            public override int Compare(IRandoItem x, IRandoItem y)
            {
                return x.Priority.CompareTo(y.Priority);
            }
        }

        private class RandoLocationComparer : Comparer<IRandoLocation>
        {
            public override int Compare(IRandoLocation x, IRandoLocation y)
            {
                return x.Priority.CompareTo(y.Priority);
            }
        }

    }
}

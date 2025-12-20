namespace RandomizerCore.StringParsing
{
    internal static class Util
    {
        public static IEnumerable<T> PartialZip<T>(IEnumerable<T> left, IEnumerable<T> right, Dictionary<(T, T), T> map) 
            => left.SelectMany(l => 
                    right.Where(r => map.ContainsKey((l, r)))
                         .Select(r => map[(l, r)]));
        public static IEnumerable<T> PartialSelect<T>(IEnumerable<T> source, Dictionary<T, T> map)
            => source.Where(map.ContainsKey).Select(t => map[t]);
    }
}

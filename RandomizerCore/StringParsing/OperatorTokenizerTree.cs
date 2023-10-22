namespace RandomizerCore.StringParsing
{
    internal class OperatorTokenizerTree
    {
        private Dictionary<char, OperatorTokenizerTree> children = new();

        public string? Value { get; private set; } = null;
        public ICollection<char> Candidates => children.Keys;

        public void Insert(string str)
        {
            char first = str[0];
            if (!children.TryGetValue(first, out OperatorTokenizerTree child))
            {
                child = new OperatorTokenizerTree();
                child.Value = (Value ?? "") + first;
                children[first] = child;
                // we don't need to go deeper
                if (str.Length == 1)
                {
                    return;
                }
            }
            else if (str.Length == 1)
            {
                // this is a case where we have a duplicate of the same operator, throw a fit
                throw new InvalidOperationException($"Duplicate operator defined: {(Value ?? "") + first}");
            }

            string rest = str[1..];
            child.Insert(rest);
        }

        public OperatorTokenizerTree Advance(char ch) => children[ch];
    }
}

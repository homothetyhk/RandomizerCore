using RandomizerCore.Extensions;

namespace RandomizerCore.StringLogic
{
    /// <summary>
    /// Class for converting RPN logic to DNF logic, which uses pooled lists to reduce memory usage.
    /// </summary>
    [Obsolete]
    internal class DNFConverter
    {
        private readonly Stack<List<TermToken>> listPool = new();
        private readonly Stack<List<List<TermToken>>> outerListPool = new();
        private readonly Stack<List<List<TermToken>>> evaluationStack = new();
        /// <summary>
        /// The output of Convert. Is recycled 
        /// </summary>
        public List<List<TermToken>>? Result { get => _result; }
        private List<List<TermToken>>? _result;

        private void Push(TermToken tt)
        {
            if (!listPool.TryPop(out List<TermToken>? l)) l = new();
            l.Add(tt);
            if (!outerListPool.TryPop(out List<List<TermToken>>? ll)) ll = new();
            ll.Add(l);
            evaluationStack.Push(ll);
        }
        private void Add()
        {
            var l1 = evaluationStack.Pop();
            evaluationStack.Peek().AddRange(l1);
            RecycleOuter(l1);
        }
        private void Multiply()
        {
            var l2 = evaluationStack.Pop();
            var l1 = evaluationStack.Pop();
            if (!outerListPool.TryPop(out List<List<TermToken>>? res)) res = new();

            int total = l1.Count * l2.Count;
            for (int i = 0; i < total; i++)
            {
                if (!listPool.TryPop(out List<TermToken>? l)) l = new();
                res.Add(l);
            }

            for (int i = 0; i < l1.Count; i++)
            {
                int r = i * l2.Count;
                for (int j = 0; j < l2.Count; j++)
                {
                    res[r + j].AddRange(l1[i]);
                    res[r + j].AddRange(l2[j]);
                }
            }

            RecycleAll(l1);
            RecycleAll(l2);
            evaluationStack.Push(res);
        }

        private void Recycle(List<TermToken> list)
        {
            list.Clear();
            listPool.Push(list);
        }
        private void RecycleAll(List<List<TermToken>> list)
        {
            for (int i = 0; i < list.Count; i++)
            {
                Recycle(list[i]);
            }
            list.Clear();
            outerListPool.Push(list);
        }
        private void RecycleOuter(List<List<TermToken>> list)
        {
            list.Clear();
            outerListPool.Push(list);
        }

        /// <summary>
        /// Reduces the number of pooled lists.
        /// </summary>
        public void Trim()
        {
            _result = null;
            outerListPool.Clear();
            listPool.Clear();
        }

        public void Convert(IEnumerable<LogicToken> tokens)
        {
            if (_result is not null) RecycleAll(_result);
            _result = null;
            foreach (LogicToken token in tokens)
            {
                switch (token)
                {
                    case TermToken tt:
                        Push(tt);
                        break;
                    case OperatorToken op when op.OperatorType == OperatorType.OR:
                        Add();
                        break;
                    case OperatorToken op when op.OperatorType == OperatorType.AND:
                        Multiply();
                        break;
                }
            }
            if (evaluationStack.Count != 1)
            {
                evaluationStack.Clear();
                throw new ArgumentException(nameof(tokens));
            }
            _result = evaluationStack.Pop();
        }
    }
}

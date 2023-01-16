using RandomizerCore.Logic;
using System.Text;

namespace RandomizerCore.Exceptions
{
    /// <summary>
    /// Exception which indicates that changing the order in which items were added to progression resulted in unexpected loss of progress.
    /// </summary>
    public class CommutativityFailureException : Exception
    {
        public CommutativityFailureException()
        {
        }

        public CommutativityFailureException(string message) : base(message)
        {
        }

        internal CommutativityFailureException(string message, ProgressionManager pm, IEnumerable<IEnumerable<IRandoItem>> gtList, IEnumerable<IEnumerable<IRandoItem>> ltList) : 
            base(RunDiagnostic(message, pm, gtList, ltList))
        {
        }

        public CommutativityFailureException(string message, Exception innerException) : base(message, innerException)
        {
        }

        private static string RunDiagnostic(string message, ProgressionManager pm, IEnumerable<IEnumerable<IRandoItem>> gtList, IEnumerable<IEnumerable<IRandoItem>> ltList)
        {
            ProgressionData snapshot = pm.GetSnapshot();
            StringBuilder sb = new();
            sb.AppendLine(message);
            sb.AppendLine("Passing item order: " + string.Join(", ", gtList.Select(i => "[" + string.Join(", ", i.Select(li => li.Name)) + "]")));
            sb.AppendLine("Failing item order: " + string.Join(", ", ltList.Select(i => "[" + string.Join(", ", i.Select(li => li.Name)) + "]")));

            if (pm.Temp) pm.RemoveTempItems();

            pm.mu.StopUpdating();
            bool located = false;
            bool item = false;
            pm.AfterAddItem += AfterAddItem;
            pm.AfterAddRange += AfterAddRange;

            try
            {
                pm.mu.StartUpdating();
                foreach (IEnumerable<IRandoItem> l in gtList) pm.Add(l);

                if (!located)
                {
                    sb.AppendLine("Attempting additional update sweeps for untracked term dependencies.");
                    do
                    {
                        item = false;
                        pm.mu.DoUpdateAll();
                    }
                    while (item && !located);
                }
            }
            catch (Exception) { }

            sb.AppendLine("Final diff with pass and fail:");
            sb.AppendLine(ProgressionManager.Diff(pm, snapshot));

            if (!located)
            {
                sb.AppendLine("Unable to locate noncommutativity source.");
            }

            return sb.ToString();

            void AfterAddItem(ILogicItem li)
            {
                item = true;
                if (located) return;

                List<Term> terms = ProgressionManager.GetDiffTerms(pm, snapshot, StringLogic.ComparisonType.GT);
                if (terms.Count > 0)
                {
                    if (pm.mu.Current is object o)
                    {
                        sb.AppendLine($"Noncommutativity tracked to update for entry {o} leading to item {li.Name}, affecting terms:");
                    }
                    else
                    {
                        sb.AppendLine($"Noncommutativity tracked to item {li.Name}, affecting terms:");
                    }
                    foreach (Term t in terms)
                    {
                        sb.AppendLine($"  {t.Name}: {(t.Type == TermType.State ? pm.lm.StateManager.PrettyPrint(pm.GetState(t)) : pm.Get(t))} > {(t.Type == TermType.State ? pm.lm.StateManager.PrettyPrint(snapshot.GetState(t)) : snapshot.GetValue(t))}");
                    }
                    located = true;
                }
            }
            void AfterAddRange(IEnumerable<ILogicItem> li)
            {
                item = true;
                if (located) return;

                List<Term> terms = ProgressionManager.GetDiffTerms(pm, snapshot, StringLogic.ComparisonType.GT);
                if (terms.Count > 0)
                {
                    if (pm.mu.Current is object o)
                    {
                        sb.AppendLine($"Noncommutativity tracked to update for entry {o} leading to items {string.Join(", ", li.Select(i => i.Name))}, affecting terms:");
                    }
                    else
                    {
                        sb.AppendLine($"Noncommutativity tracked to items {string.Join(", ", li.Select(i => i.Name))}, affecting terms:");
                    }
                    foreach (Term t in terms)
                    {
                        sb.AppendLine($"  {t.Name}: {(t.Type == TermType.State ? pm.lm.StateManager.PrettyPrint(pm.GetState(t)) : pm.Get(t))} > {(t.Type == TermType.State ? pm.lm.StateManager.PrettyPrint(snapshot.GetState(t)) : snapshot.GetValue(t))}");
                    }
                    located = true;
                }
            }
        }
    }
}

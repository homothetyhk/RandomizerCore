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

        public CommutativityFailureException(string message, Randomization.CombinedItemSelector selector, ProgressionManager pm) : base(AugmentMessage(message, selector, pm))
        {
        }

        public CommutativityFailureException(string message, Exception innerException) : base(message, innerException)
        {
        }

        private static string AugmentMessage(string orig, Randomization.CombinedItemSelector selector, ProgressionManager pm)
        {
            StringBuilder sb = new();
            sb.AppendLine(orig);
            sb.AppendLine("Accepted items: " + string.Join(", ", selector.GetAcceptedItems().Select(i => i.Name)));
            sb.AppendLine("Proposed items: " + string.Join(", ", selector.GetProposedItems().Select(i => i.Name)));
            sb.AppendLine(pm.Dump());
            return sb.ToString();
        }
    }
}

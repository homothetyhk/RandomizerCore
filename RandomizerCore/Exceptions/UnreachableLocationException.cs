using RandomizerCore.Logic;
using RandomizerCore.Randomization;
using System.Text;

namespace RandomizerCore.Exceptions
{
    /// <summary>
    /// Exception which indicates that a location was unreachable after all items were placed.
    /// </summary>
    public class UnreachableLocationException : Exception
    {
        public readonly List<IRandoLocation>[]? unreachableLocations;
        public readonly RandomizationStage? stage;
        public readonly TempState state;
        public readonly ProgressionManager? pm;


        public UnreachableLocationException() { }
        public UnreachableLocationException(string message) : base(message) { }
        public UnreachableLocationException(string message, Exception inner) : base(message, inner) { }
        public UnreachableLocationException(List<IRandoLocation>[] unreachableLocations, RandomizationStage stage, TempState state, ProgressionManager pm)
            : base()
        {
            this.unreachableLocations = unreachableLocations;
            this.stage = stage;
            this.state = state;
            this.pm = pm;
        }

        public string GetVerboseMessage()
        {
            if (unreachableLocations == null || stage == null || pm == null) return string.Empty;

            StringBuilder sb = new();
            sb.AppendLine($"Unreachable locations found during randomization stage {stage.label} ({state}):");
            for (int i = 0; i < unreachableLocations.Length; i++)
            {
                List<IRandoLocation> list = unreachableLocations[i];
                if (list.Count > 0)
                {
                    sb.Append("  ");
                    sb.Append(stage.groups[i].Label);
                    sb.Append(": ");
                    sb.AppendLine(string.Join(", ", list.Select(l => l.Name)));
                }
            }
            sb.AppendLine(pm.Dump());

            return sb.ToString();
        }
    }
}

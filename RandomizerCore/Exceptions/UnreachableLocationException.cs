using RandomizerCore.Randomization;
using System.Text;

namespace RandomizerCore.Exceptions
{
    public class UnreachableLocationException : Exception
    {
        public UnreachableLocationException() { }
        public UnreachableLocationException(string message) : base(message) { }
        public UnreachableLocationException(string message, Exception inner) : base(message, inner) { }
        public UnreachableLocationException(List<IRandoLocation>[] unreachableLocations, RandomizationGroup[] groups) 
            : base(CreateMessage(unreachableLocations, groups)) { }

        public static string CreateMessage(List<IRandoLocation>[] unreachableLocations, RandomizationGroup[] groups)
        {
            StringBuilder sb = new();
            sb.AppendLine("Unreachable locations found during randomization:");
            for (int i = 0; i < unreachableLocations.Length; i++)
            {
                List<IRandoLocation> list = unreachableLocations[i];
                if (list.Count > 0)
                {
                    sb.Append("  ");
                    sb.Append(groups[i].Label);
                    sb.Append(": ");
                    sb.AppendLine(string.Join(", ", list.Select(l => l.Name)));
                }
            }
            return sb.ToString();
        }
    }
}

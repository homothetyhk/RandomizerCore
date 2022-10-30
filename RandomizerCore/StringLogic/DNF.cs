using System.Net;

namespace RandomizerCore.StringLogic
{
    /// <summary>
    /// Utility methods for dealing with logic in DNF form.
    /// </summary>
    public static class DNF
    {
        public static string ToInfix(List<TermToken> conjunction)
        {
            return string.Join(" + ", conjunction.Select(t => t.Write()));
        }

        public static string ToInfix(List<List<TermToken>> dnf)
        {
            return string.Join(" | ", dnf.Select(c => string.Join(" + ", c.Select(t => t.Write()))));
        }
    }
}

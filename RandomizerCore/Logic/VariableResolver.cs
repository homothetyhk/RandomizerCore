using RandomizerCore.Logic.StateLogic;
using System.Reflection;

namespace RandomizerCore.Logic
{
    public class VariableResolver
    {
        public VariableResolver? Inner { get; init; }

        public bool CanMatch(LogicManager lm, string term) => TryMatch(lm, term, out LogicVariable _);

        public virtual bool TryMatch(LogicManager lm, string term, out LogicVariable variable)
        {
            if (int.TryParse(term, out int value))
            {
                variable = new ConstantInt(value);
                return true;
            }
            #pragma warning disable CS0612 // Type or member is obsolete
            if (TryMatch(lm, term, out LogicInt @int))
            {
                variable = @int;
                return true;
            }
            #pragma warning restore CS0612 // Type or member is obsolete
            if (Inner != null)
            {
                return Inner.TryMatch(lm, term, out variable);
            }
            if (term == StartStateProvider.Prefix)
            {
                variable = new StartStateProvider();
                return true;
            }
            variable = null;
            return false;
        }
        [Obsolete]
        public virtual bool TryMatch(LogicManager lm, string term, out LogicInt variable)
        {
            variable = default;
            return false;
        }

        /// <summary>
        /// Matches a variable identified via a prefix. The input must either equal the prefix exactly, or consist of the prefix, followed by square brackets enclosing a comma-separated list.
        /// Outputs the comma-separated parameters list.
        /// </summary>
        public static bool TryMatchPrefix(string term, string prefix, out string[] parameters)
        {
            if (term.StartsWith(prefix))
            {
                if (term.Length == prefix.Length)
                {
                    parameters = Array.Empty<string>();
                    return true;
                }
                else if (term[prefix.Length] == '[' && term[^1] == ']')
                {
                    parameters = term[(prefix.Length+1)..^1].Split(',');
                    return true;
                }
            }
            parameters = null; 
            return false;
        }
    }
}

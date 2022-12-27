using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// Class which parses terms in logic as LogicVariables.
    /// </summary>
    public class VariableResolver
    {
        /// <summary>
        /// A nested VariableResolver, which runs after the parent VariableResolver in TryMatch.
        /// </summary>
        public VariableResolver? Inner { get; init; }

        /// <summary>
        /// Returns true if the term can be matched to a LogicVariable.
        /// </summary>
        public bool CanMatch(LogicManager lm, string term) => TryMatch(lm, term, out LogicVariable _);

        /// <summary>
        /// Returns true if the term can be matched to a LogicVariable, and outputs the variable.
        /// </summary>
        public virtual bool TryMatch(LogicManager lm, string term, out LogicVariable variable)
        {
            if (int.TryParse(term, out int value))
            {
                variable = new ConstantInt(value);
                return true;
            }
            if (StartStateProvider.TryMatch(lm, term, out variable)) return true;
            if (StateAccessVariable.TryMatch(lm, term, out variable)) return true;

            #pragma warning disable CS0618 // Type or member is obsolete
            if (TryMatch(lm, term, out LogicInt @int))
            {
                variable = @int;
                return true;
            }
            #pragma warning restore CS0618 // Type or member is obsolete

            if (Inner != null)
            {
                return Inner.TryMatch(lm, term, out variable);
            }

            variable = null;
            return false;
        }
        [Obsolete("Use LogicVariable overload of TryMatch instead.")]
        public virtual bool TryMatch(LogicManager lm, string term, out LogicInt variable)
        {
            variable = default;
            return false;
        }

        /// <summary>
        /// Matches a variable identified via a prefix. The input must either equal the prefix exactly, or consist of the prefix, followed by square brackets enclosing a comma-separated list. Subexpressions within square brackets will not be split.
        /// <br/>Outputs the comma-separated parameters list.
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
                    int head = prefix.Length + 1;
                    int depth = 1;
                    List<string> prmtrs = new();
                    for (int i = head; i < term.Length; i++)
                    {
                        switch (term[i])
                        {
                            default:
                                continue;
                            case '[':
                                depth++;
                                continue;
                            case ']':
                                depth--;
                                if (depth == 0)
                                {
                                    prmtrs.Add(term[head..i]);
                                    head = i + 1;
                                }
                                continue;
                            case ',':
                                if (depth == 1)
                                {
                                    prmtrs.Add(term[head..i]);
                                    head = i + 1;
                                }
                                continue;
                        }
                    }
                    parameters = prmtrs.ToArray();
                    return true;
                }
            }
            parameters = null; 
            return false;
        }
    }
}

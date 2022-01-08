using System.Text.RegularExpressions;

namespace RandomizerCore.Logic
{
    public class VariableResolver
    {
        public virtual bool TryMatch(LogicManager lm, string term, out LogicInt variable)
        {
            Match match;

            if (int.TryParse(term, out int value))
            {
                variable = new ConstantInt(value);
                return true;
            }

            match = Regex.Match(term, @"^\$NotchCost\[(.+)\]$");
            if (match.Success)
            {
                variable = new NotchCostInt(match.Groups[1].Value.Split(',').Select(s => int.Parse(s)).ToArray());
                return true;
            }

            variable = null;
            return false;
        }
    }
}

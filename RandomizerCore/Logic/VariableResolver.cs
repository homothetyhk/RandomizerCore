using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Text.RegularExpressions;
using static RandomizerCore.LogHelper;

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
                Log($"Matched term {term} as NotchCostInt.");
                variable = new NotchCostInt(match.Groups[1].Value.Split(',').Select(s => int.Parse(s)).ToArray());
                return true;
            }

            variable = null;
            return false;
        }
    }
}

using System;
using System.Collections.Generic;

namespace RandomizerCore.StringLogic
{
    /// <summary>
    /// Object which evaluates tokenized logic using a dictionary of recognized terms and their values.
    /// </summary>
    public class DictPM : StringPM
    {
        private readonly Dictionary<string, int> terms = new();

        public void SetBool(string name, bool value)
        {
            if (terms.TryGetValue(name, out int level))
            {
                if (level > 0 & value || level <= 0 && !value) return;
            }

            terms[name] = value ? 1 : 0;
        }

        public void SetInt(string name, int value)
        {
            terms[name] = value;
        }

        public void IncrementInt(string name, int incr)
        {
            if (!terms.TryGetValue(name, out int level)) level = 0;
            terms[name] = level + incr;
        }

        public override bool Evaluate(TermToken token)
        {
            if (token is ConstToken bt)
            {
                return bt.Value;
            }
            else if (token is SimpleToken st)
            {
                return terms[st.Name] > 0;
            }
            else if (token is ComparisonToken ct)
            {
                int left;
                int right;
                if (!terms.TryGetValue(ct.Left, out left) && !int.TryParse(ct.Left, out left))
                {
                    throw new ArgumentException($"Unable to convert {ct.Left} to term or integer.");
                }
                if (!terms.TryGetValue(ct.Right, out right) && !int.TryParse(ct.Right, out right))
                {
                    throw new ArgumentException($"Unable to convert {ct.Right} to term or integer.");
                }

                return ct.ComparisonType switch
                {
                    ComparisonType.LT => left < right,
                    ComparisonType.GT => left > right,
                    _ => left == right,
                };

            }
            else if (token is MacroToken mt)
            {
                return Evaluate(mt.Value);
            }
            throw new ArgumentException($"Unable to evaluate TermToken: {token}");
        }
    }

}

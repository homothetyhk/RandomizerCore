using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using static RandomizerCore.LogHelper;

namespace RandomizerCore.StringLogic
{
    public class LogicProcessor
    {
        private readonly Dictionary<string, LogicToken> tokenPool;

        public LogicProcessor() 
        {
            tokenPool = new()
            {
                { "TRUE", ConstToken.True },
                {  "ANY", ConstToken.True },
                { "FALSE", ConstToken.False },
                { "NONE", ConstToken.False },
                { OperatorToken.AND.Symbol, OperatorToken.AND },
                { OperatorToken.OR.Symbol, OperatorToken.OR },
            };
        }

        public void LogSelf()
        {
            return;
            Log("Logging token pool contents...");
            foreach (var kvp in tokenPool) Log($"{kvp.Key}: {kvp.Value}");
            Log("End of token pool contents.");
        }

        public void SetMacro(Dictionary<string, string> macros)
        {
            if (macros == null) return;
            foreach (var kvp in macros) tokenPool[kvp.Key] = new MacroToken(kvp.Key, new(kvp.Value, tokenPool));
        }

        public void SetMacro(string key, string infix)
        {
            tokenPool[key] = new MacroToken(key, new(infix, tokenPool));
        }

        public void SetMacro(string key, LogicClause c)
        {
            tokenPool[key] = new MacroToken(key, c);
        }

        public void SetMacro(KeyValuePair<string, string> kvp)
        {
            tokenPool[kvp.Key] = new MacroToken(kvp.Key, new(kvp.Value, tokenPool));
        }

        public ComparisonToken GetComparisonToken(ComparisonType comparisonType, string left, string right)
        {
            char symbol = comparisonType switch
            {
                ComparisonType.EQ => '=',
                ComparisonType.LT => '<',
                ComparisonType.GT => '>',
                _ => throw new NotImplementedException(),
            };
            string name = $"{left}{symbol}{right}";
            if (tokenPool.TryGetValue(name, out LogicToken lt)) return (ComparisonToken)lt;
            else
            {
                ComparisonToken ct = new(comparisonType, left, right);
                tokenPool.Add(name, ct);
                return ct;
            }
        }

        public TermToken GetTermToken(string name)
        {
            if (tokenPool.TryGetValue(name, out LogicToken lt)) return (TermToken)lt;
            else
            {
                TermToken tt = new SimpleToken(name);
                tokenPool.Add(name, tt);
                return tt;
            }
        }


        public LogicClause ParseInfixToClause(string infix) => new(infix, tokenPool);
        public List<LogicToken> ParseInfixToList(string infix) => Infix.Tokenize(infix, tokenPool);
    }
}

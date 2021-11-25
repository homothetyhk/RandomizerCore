using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using static RandomizerCore.LogHelper;

namespace RandomizerCore.StringLogic
{
    public class LogicProcessor : ITokenSource, IMacroSource
    {
        private static readonly Dictionary<string, LogicToken> globalTokens = new()
        {
            { "TRUE", ConstToken.True },
            { "ANY", ConstToken.True },
            { "FALSE", ConstToken.False },
            { "NONE", ConstToken.False },
            { OperatorToken.AND.Symbol, OperatorToken.AND },
            { OperatorToken.OR.Symbol, OperatorToken.OR },
            { "ORIG", new SimpleToken("ORIG") },
        };
        private readonly Dictionary<string, LogicToken> tokenPool;
        private readonly Dictionary<string, LogicClause> macros;

        public LogicProcessor() 
        {
            tokenPool = new();
            macros = new();
        }

        public void LogSelf()
        {
            return;
            Log("Logging token pool contents...");
            foreach (var kvp in tokenPool) Log($"{kvp.Key}: {kvp.Value}");
            Log("End of token pool contents.");
        }

        public void SetMacro(Dictionary<string, string> newMacros)
        {
            if (newMacros == null) return;
            foreach (var kvp in newMacros)
            {
                SetMacro(kvp);
            }
        }
        public void SetMacro(string key, string infix) => SetMacro(key, ParseInfixToClause(infix));
        public void SetMacro(KeyValuePair<string, string> kvp) => SetMacro(kvp.Key, ParseInfixToClause(kvp.Value));
        public void SetMacro(string key, LogicClause c)
        {
            macros[key] = c;
            if (!tokenPool.TryGetValue(key, out LogicToken lt) || lt is not MacroToken)
            {
                tokenPool[key] = new MacroToken(key, this);
            }
        }

        public LogicClause GetMacro(string name)
        {
            return macros[name];
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
            if (globalTokens.TryGetValue(name, out LogicToken lt) || tokenPool.TryGetValue(name, out lt)) return (TermToken)lt;
            else
            {
                TermToken tt = new SimpleToken(name);
                tokenPool.Add(name, tt);
                return tt;
            }
        }

        public LogicClause ParseInfixToClause(string infix) => new(infix, this);
        public LogicClauseBuilder ParseInfixToBuilder(string infix) => new(Infix.Tokenize(infix, this));
        public List<LogicToken> ParseInfixToList(string infix) => Infix.Tokenize(infix, this);
    }
}

using Newtonsoft.Json;
using System.Collections.ObjectModel;

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

        [JsonIgnore] public readonly ReadOnlyDictionary<string, LogicToken> TokenPool;
        [JsonIgnore] public readonly ReadOnlyDictionary<string, LogicClause> Macros;
        [JsonProperty("macros")] private Dictionary<string, string>? __macros => Macros?.ToDictionary(kvp => kvp.Key, kvp => kvp.Value.ToInfix());

        public LogicProcessor() 
        {
            tokenPool = new();
            macros = new();

            TokenPool = new(tokenPool);
            Macros = new(macros);
        }

        [JsonConstructor]
        public LogicProcessor(Dictionary<string, string> macros) : this()
        {
            SetMacro(macros);
        }

        public LogicProcessor(LogicProcessor source)
        {
            tokenPool = new(source.tokenPool);
            macros = new(source.macros);

            TokenPool = new(tokenPool);
            Macros = new(macros);
        }

        public void SetMacro(Dictionary<string, string> newMacros)
        {
            if (newMacros == null) return;
            foreach (var kvp in newMacros)
            {
                SetMacro(kvp);
            }
        }
        public void SetMacro(string key, string infix)
        {
            LogicClause lc;
            try
            {
                lc = ParseInfixToClause(infix);
            }
            catch (Exception e)
            {
                throw new InvalidOperationException($"Logic \"{infix}\" for macro {key} is malformed.", e);
            }
            SetMacro(key, lc);
        }

        public void SetMacro(KeyValuePair<string, string> kvp)
        {
            LogicClause lc;
            try
            {
                lc = ParseInfixToClause(kvp.Value);
            }
            catch (Exception e)
            {
                throw new InvalidOperationException($"Logic \"{kvp.Value}\" for macro {kvp.Key} is malformed.", e);
            }

            SetMacro(kvp.Key, lc);
        }

        public void SetMacro(string key, LogicClause c)
        {
            if (tokenPool.TryGetValue(key, out LogicToken lt) && lt is not MacroToken)
            {
                throw new ArgumentException($"Key {key} cannot be used as a macro since it is already in use for {lt}");
            }

            macros[key] = c;
            if (lt is null) tokenPool[key] = new MacroToken(key, this);
        }

        public bool IsMacro(string name) => macros.ContainsKey(name);

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
                if (name[0] == '*')
                {
                    ReferenceToken rt = new(name[1..]);
                    tokenPool.Add(name, rt);
                    return rt;
                }

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

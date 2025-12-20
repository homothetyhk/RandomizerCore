using RandomizerCore.Logic;
using System.Collections.ObjectModel;

namespace RandomizerCore.StringLogic
{
    [Obsolete]
    public class LogicProcessor : ITokenSource, IMacroSource
    {
        [Obsolete]
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
        [Obsolete] 
        private readonly Dictionary<string, LogicToken> tokenPool;
        private readonly Dictionary<string, LogicClause> macros;

        [Obsolete] 
        public readonly ReadOnlyDictionary<string, LogicToken> TokenPool;
        public readonly ReadOnlyDictionary<string, LogicClause> Macros;

        private readonly LogicManagerBuilder? lmb;
        private readonly LogicManager? lm;

        public LogicProcessor() 
        {
#pragma warning disable CS0612 // Type or member is obsolete
            tokenPool = [];
            TokenPool = new(tokenPool);
#pragma warning restore CS0612 // Type or member is obsolete

            macros = [];
            Macros = new(macros);
        }

        public LogicProcessor(Dictionary<string, string> macros) : this()
        {
            SetMacro(macros);
        }

        public LogicProcessor(LogicProcessor source)
        {
#pragma warning disable CS0612 // Type or member is obsolete
            tokenPool = new(source.tokenPool);
            TokenPool = new(tokenPool);
#pragma warning restore CS0612 // Type or member is obsolete

            macros = new(source.macros);
            Macros = new(macros);
        }

        internal LogicProcessor(LogicManagerBuilder lmb) : this()
        {
            this.lmb = lmb;
        }

        internal LogicProcessor(LogicManager lm) : this()
        {
            this.lm = lm;
        }

        [Obsolete]
        private void DefineMacroToken(string key)
        {
            if (tokenPool.TryGetValue(key, out LogicToken lt))
            {
                if (lt is not MacroToken) throw new ArgumentException($"Key {key} cannot be used as a macro since it is already in use for {lt}");
            }
            else
            {
                tokenPool.Add(key, new MacroToken(key, this));
            }
        }

        [Obsolete]
        private void DefineMacroTokens(IEnumerable<string> keys)
        {
            foreach (string key in keys) DefineMacroToken(key);
        }

        [Obsolete]
        public void SetMacro(Dictionary<string, string> newMacros)
        {
            if (newMacros == null) return;
            DefineMacroTokens(newMacros.Keys);

            foreach (var kvp in newMacros)
            {
                SetMacro(kvp);
            }
        }

        [Obsolete]
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

        [Obsolete]
        public void SetMacro(KeyValuePair<string, string> kvp) => SetMacro(kvp.Key, kvp.Value);

        [Obsolete]
        public void SetMacro(string key, LogicClause c)
        {
            if (lm is not null) throw new InvalidOperationException("Cannot set macro on LogicProcessor; LP is attached to an immutable LogicManager.");
            DefineMacroToken(key);
            macros[key] = c;
            lmb?.AddMacro(new(key, c.ToInfix()));
        }

        public bool IsMacro(string name) => macros.ContainsKey(name);

        public LogicClause GetMacro(string name)
        {
            if (lm is not null) return lm.MacroLookup[name].Logic;
            if (lmb is not null) return lmb.MacroLookup[name];
            return macros[name];
        }

        [Obsolete]
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

        [Obsolete]
        private static readonly HashSet<char> illegalSimpleTokenChars = ['|', '+', '<', '>', '=', '?', '(', ')', '*', '/'];

        [Obsolete]
        public TermToken GetTermToken(string name)
        {
            if (globalTokens.TryGetValue(name, out LogicToken lt) || tokenPool.TryGetValue(name, out lt)) return (TermToken)lt;
            else
            {
                if (name[^1] == '/')
                {
                    TermToken inner = GetTermToken(name[..^1]);
                    if (inner is SimpleToken st)
                    {
                        ProjectedToken pt = new(st);
                        tokenPool.Add(name, pt);
                        return pt;
                    }
                    else if (inner is ReferenceToken rt)
                    {
                        ProjectedToken pt = new(rt);
                        tokenPool.Add(name, pt);
                        return pt;
                    }
                    else throw new ArgumentException($"Cannot apply projection operator to token {inner.Write()} of type {inner.GetType()}.");
                }

                if (name[0] == '*')
                {
                    ReferenceToken rt = new(name[1..]);
                    tokenPool.Add(name, rt);
                    return rt;
                }

                if (name.Any(illegalSimpleTokenChars.Contains)) throw new ArgumentException($"Failed to convert {name} to token due to illegal characters.");

                TermToken tt = new SimpleToken(name);
                tokenPool.Add(name, tt);
                return tt;
            }
        }

        [Obsolete]
        public LogicClause ParseInfixToClause(string infix) => new(infix);
        [Obsolete]
        public LogicClauseBuilder ParseInfixToBuilder(string infix) => new(infix);
        [Obsolete]
        public List<LogicToken> ParseInfixToList(string infix) => Infix.Tokenize(infix, this);
    }
}

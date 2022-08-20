using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RandomizerCore.Json;
using RandomizerCore.LogicItems;
using RandomizerCore.StringLogic;

namespace RandomizerCore.Logic
{
    public class LogicManagerBuilder
    {
        public LogicManagerBuilder()
        {
            terms = new();
            termLookup = new();
            LP = new();
            VariableResolver = new();
            PrefabItems = new();
            TemplateItems = new();
            UnparsedItems = new();
            LogicLookup = new();
            Waypoints = new();
            Transitions = new();
        }

        public LogicManagerBuilder(LogicManagerBuilder source)
        {
            terms = new(source.terms);
            termLookup = new(source.termLookup);
            LP = new(source.LP);
            VariableResolver = source.VariableResolver;
            PrefabItems = new(source.PrefabItems);
            TemplateItems = new(source.TemplateItems);
            UnparsedItems = new(source.UnparsedItems);
            LogicLookup = new(source.LogicLookup);
            Waypoints = new(source.Waypoints);
            Transitions = new(source.Transitions);
        }

        public LogicManagerBuilder(LogicManager source)
        {
            terms = new(source.Terms);
            termLookup = new(source.TermLookup);
            LP = new(source.LP);
            VariableResolver = source.VariableResolver;
            PrefabItems = new(source.ItemLookup);
            TemplateItems = new();
            UnparsedItems = new();
            LogicLookup = source.LogicLookup.ToDictionary(kvp => kvp.Key, kvp => kvp.Value.ToLogicClause());
            Waypoints = new(source.Waypoints.Select(w => w.Name));
            Transitions = new(source.TransitionLookup.Values.Select(lt => lt.Name));
        }


        private readonly List<Term> terms;
        public IReadOnlyList<Term> Terms => terms;

        private readonly Dictionary<string, Term> termLookup;
        public IReadOnlyDictionary<string, Term> TermLookup => termLookup;

        public LogicProcessor LP { get; set; }
        public VariableResolver VariableResolver { get; set; }
        public readonly Dictionary<string, LogicItem> PrefabItems;
        public readonly Dictionary<string, ILogicItemTemplate> TemplateItems;
        public readonly Dictionary<string, JObject> UnparsedItems;
        public readonly Dictionary<string, LogicClause> LogicLookup;
        public readonly HashSet<string> Waypoints;
        public readonly HashSet<string> Transitions;

        /// <summary>
        /// Returns whether the string is a key in the term lookup.
        /// </summary>
        public bool IsTerm(string value)
        {
            if (value == null) throw new ArgumentNullException(nameof(value));
            return termLookup.ContainsKey(value);
        }

        /// <summary>
        /// If the string is a key in the term lookup, returns the corresponding term. Otherwise, creates, saves, and returns a new term.
        /// </summary>
        public Term GetOrAddTerm(string value)
        {
            if (value == null) throw new ArgumentNullException(nameof(value));

            if (termLookup.TryGetValue(value, out Term t)) return t;
            t = new(terms.Count, value);
            terms.Add(t);
            return termLookup[value] = t;
        }

        /// <summary>
        /// Adds the LogicItem to the builder's dictionary. Overwrites any existing item with the same name.
        /// </summary>
        /// <param name="item"></param>
        public void AddItem(LogicItem item)
        {
            PrefabItems[item.Name] = item;
            TemplateItems.Remove(item.Name);
            UnparsedItems.Remove(item.Name);
        }

        /// <summary>
        /// Adds the item template to the builder's dictionary. Overwrites any existing item with the same name.
        /// </summary>
        public void AddTemplateItem(ILogicItemTemplate item)
        {
            TemplateItems[item.Name] = item;
            PrefabItems.Remove(item.Name);
            UnparsedItems.Remove(item.Name);
        }

        /// <summary>
        /// Adds the JLINQ representation of the LogicItem to the builder's dictionary. Overwrites any existing item with the same name.
        /// </summary>
        public void AddUnparsedItem(JObject item)
        {
            string name = item.Value<string>("Name");
            UnparsedItems[name] = item;
            PrefabItems.Remove(name);
            TemplateItems.Remove(name);
        }

        /// <summary>
        /// Adds the RawLogicDef as a new waypoint. Overwrites any existing logic with the same name.
        /// </summary>
        public void AddWaypoint(RawLogicDef def)
        {
            GetOrAddTerm(def.name);
            LogicLookup[def.name] = LP.ParseInfixToClause(def.logic);
            Waypoints.Add(def.name);
        }

        /// <summary>
        /// Adds the RawLogicTransition as a new transition. Overwrites any existing logic with the same name.
        /// </summary>
        public void AddTransition(RawLogicDef def)
        {
            GetOrAddTerm(def.name);
            LogicLookup[def.name] = LP.ParseInfixToClause(def.logic);
            Transitions.Add(def.name);
        }

        /// <summary>
        /// Adds the RawLogicDef for general use. Overwrites any existing logic with the same name.
        /// </summary>
        public void AddLogicDef(RawLogicDef def)
        {
            LogicLookup[def.name] = LP.ParseInfixToClause(def.logic);
        }

        /// <summary>
        /// If the input contains the ORIG token and the logic def is already defined, substitutes the old value for ORIG in the input, and overwrites the old logic.
        /// <br/>If the input does not contain the ORIG token, is equivalent to AddLogicDef.
        /// </summary>
        public void DoLogicEdit(RawLogicDef def)
        {
            LogicClauseBuilder lcb = LP.ParseInfixToBuilder(def.logic);
            if (lcb.Tokens.Any(lt => lt is SimpleToken st && st.Name == "ORIG"))
            {
                if (!LogicLookup.TryGetValue(def.name, out LogicClause orig))
                {
                    throw new ArgumentException($"ORIG edit requested for nonexistent logic def {def.name}: {def.logic}");
                }

                lcb.Subst(LP.GetTermToken("ORIG"), orig);
            }
            LogicLookup[def.name] = new(lcb);
        }

        /// <summary>
        /// If the input contains the ORIG token and the macro is already defined, substitutes the old value for ORIG in the input, and overwrites the old macro.
        /// <br/>If the input does not contain the ORIG token, is equivalent to LP.SetMacro.
        /// </summary>
        public void DoMacroEdit(KeyValuePair<string, string> kvp)
        {
            LogicClauseBuilder lcb = LP.ParseInfixToBuilder(kvp.Value);
            if (lcb.Tokens.Any(lt => lt is SimpleToken st && st.Name == "ORIG"))
            {
                if (!LP.IsMacro(kvp.Key))
                {
                    throw new ArgumentException($"ORIG edit requested for nonexistent macro {kvp.Key}: {kvp.Value}");
                }

                lcb.Subst(LP.GetTermToken("ORIG"), LP.GetMacro(kvp.Key));
            }
            LP.SetMacro(kvp.Key, new LogicClause(lcb));
        }

        /// <summary>
        /// Performs the requested substitution on the named logic def or macro.
        /// </summary>
        public void DoSubst(RawSubstDef def)
        {
            TermToken tt = LP.GetTermToken(def.old);
            LogicClause lc = LP.ParseInfixToClause(def.replacement);
            bool isMacro = LP.IsMacro(def.name);
            bool isLocation = LogicLookup.TryGetValue(def.name, out LogicClause orig);
            if (isMacro && isLocation)
            {
                throw new ArgumentException($"Ambiguous substituion request: \"{def.name}\" is used as a macro and as a logic def.");
            }
            else if (isMacro)
            {
                LogicClauseBuilder lcb = new(LP.GetMacro(def.name));
                lcb.Subst(tt, lc);
                LP.SetMacro(def.name, new LogicClause(lcb));
            }
            else if (isLocation)
            {
                LogicClauseBuilder lcb = new(orig);
                lcb.Subst(tt, lc);
                LogicLookup[def.name] = new(lcb);
            }
            else
            {
                throw new ArgumentException($"RawSubstDef {def} does not correspond to any known macro or logic.");
            }
        }

        public enum JsonType
        {
            Terms,
            Waypoints,
            Transitions,
            Macros,
            Items,
            Locations,
            LogicEdit,
            MacroEdit,
            LogicSubst,
            ItemTemplates,
        }

        public void DeserializeJson(JsonType type, string s)
        {
            using StringReader sr = new(s);
            using JsonTextReader jtr = new(sr);
            DeserializeJson(type, jtr);
        }

        public void DeserializeJson(JsonType type, Stream s)
        {
            using StreamReader sr = new(s);
            using JsonTextReader jtr = new(sr);
            DeserializeJson(type, jtr);
        }

        public void DeserializeJson(JsonType type, JsonTextReader jtr)
        {
            switch (type)
            {
                case JsonType.Terms:
                    foreach (string term in JsonUtil.Deserialize<string[]>(jtr) ?? Enumerable.Empty<string>())
                    {
                        GetOrAddTerm(term);
                    }
                    break;

                case JsonType.Waypoints:
                    foreach (RawLogicDef def in JsonUtil.Deserialize<RawLogicDef[]>(jtr) ?? Enumerable.Empty<RawLogicDef>())
                    {
                        AddWaypoint(def);
                    }
                    break;

                case JsonType.Transitions:
                    foreach (RawLogicDef def in JsonUtil.Deserialize<RawLogicDef[]>(jtr) ?? Enumerable.Empty<RawLogicDef>())
                    {
                        AddTransition(def);
                    }
                    break;

                case JsonType.Macros:
                    LP.SetMacro(JsonUtil.Deserialize<Dictionary<string, string>>(jtr));
                    break;

                case JsonType.Items:
                    {
                        foreach (JObject jo in JArray.Load(jtr).Cast<JObject>())
                        {
                            AddUnparsedItem(jo);
                        }
                    }
                    break;

                case JsonType.Locations:
                    foreach (RawLogicDef def in JsonUtil.Deserialize<RawLogicDef[]>(jtr) ?? Enumerable.Empty<RawLogicDef>())
                    {
                        AddLogicDef(def);
                    }
                    break;

                case JsonType.LogicEdit:
                    foreach (RawLogicDef def in JsonUtil.Deserialize<RawLogicDef[]>(jtr) ?? Enumerable.Empty<RawLogicDef>())
                    {
                        DoLogicEdit(def);
                    }
                    break;
                case JsonType.MacroEdit:
                    foreach (KeyValuePair<string, string> kvp in JsonUtil.Deserialize<Dictionary<string, string>>(jtr) ?? Enumerable.Empty<KeyValuePair<string, string>>())
                    {
                        DoMacroEdit(kvp);
                    }
                    break;

                case JsonType.LogicSubst:
                    foreach (RawSubstDef def in JsonUtil.Deserialize<List<RawSubstDef>>(jtr) ?? Enumerable.Empty<RawSubstDef>())
                    {
                        DoSubst(def);
                    }
                    break;
                case JsonType.ItemTemplates:
                    foreach (ILogicItemTemplate template in JsonUtil.Deserialize<IEnumerable<ILogicItemTemplate>>(jtr) ?? Enumerable.Empty<ILogicItemTemplate>())
                    {
                        AddTemplateItem(template);
                    }
                    break;
            }
        }

        public void DeserializeJson(JsonType type, JToken t)
        {
            switch (type)
            {
                case JsonType.Terms:
                    foreach (string term in t.ToObject<List<string>>() ?? Enumerable.Empty<string>())
                    {
                        GetOrAddTerm(term);
                    }
                    break;

                case JsonType.Waypoints:
                    foreach (RawLogicDef def in t.ToObject<List<RawLogicDef>>() ?? Enumerable.Empty<RawLogicDef>())
                    {
                        AddWaypoint(def);
                    }
                    break;

                case JsonType.Transitions:
                    foreach (RawLogicDef def in t.ToObject<List<RawLogicDef>>() ?? Enumerable.Empty<RawLogicDef>())
                    {
                        AddTransition(def);
                    }
                    break;

                case JsonType.Macros:
                    LP.SetMacro(t.ToObject<Dictionary<string, string>>());
                    break;

                case JsonType.Items:
                    {
                        foreach (JObject jo in (JArray)t)
                        {
                            AddUnparsedItem(jo);
                        }
                    }
                    break;

                case JsonType.Locations:
                    foreach (RawLogicDef def in t.ToObject<List<RawLogicDef>>() ?? Enumerable.Empty<RawLogicDef>())
                    {
                        AddLogicDef(def);
                    }
                    break;

                case JsonType.LogicEdit:
                    foreach (RawLogicDef def in t.ToObject<RawLogicDef[]>() ?? Enumerable.Empty<RawLogicDef>())
                    {
                        DoLogicEdit(def);
                    }
                    break;
                case JsonType.MacroEdit:
                    foreach (KeyValuePair<string, string> kvp in t.ToObject<Dictionary<string, string>>() ?? Enumerable.Empty<KeyValuePair<string, string>>())
                    {
                        DoMacroEdit(kvp);
                    }
                    break;
                case JsonType.LogicSubst:
                    foreach (RawSubstDef def in t.ToObject<List<RawSubstDef>>() ?? Enumerable.Empty<RawSubstDef>())
                    {
                        DoSubst(def);
                    }
                    break;
                case JsonType.ItemTemplates:
                    foreach (ILogicItemTemplate template in t.ToObject<IEnumerable<ILogicItemTemplate>>() ?? Enumerable.Empty<ILogicItemTemplate>())
                    {
                        AddTemplateItem(template);
                    }
                    break;
            }
        }

        public Term GetTerm(string term)
        {
            return termLookup[term];
        }

        public Term GetTerm(int index)
        {
            return terms[index];
        }
    }
}

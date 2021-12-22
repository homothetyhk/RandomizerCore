using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RandomizerCore.Json;
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
            UnparsedItems = new();
            LogicLookup = source.LogicLookup.ToDictionary(kvp => kvp.Key, kvp => kvp.Value.ToLogicClause());
            Waypoints = source.Waypoints.Select(w => w.Name).ToList();
            Transitions = source.TransitionLookup.Values.Select(lt => lt.data).ToList();
        }


        private readonly List<Term> terms;
        public IReadOnlyList<Term> Terms => terms;

        private readonly Dictionary<string, Term> termLookup;
        public IReadOnlyDictionary<string, Term> TermLookup => termLookup;

        public LogicProcessor LP { get; set; }
        public VariableResolver VariableResolver { get; set; }
        public readonly Dictionary<string, LogicItem> PrefabItems;
        public readonly Dictionary<string, JObject> UnparsedItems;
        public readonly Dictionary<string, LogicClause> LogicLookup;
        public readonly List<string> Waypoints;
        public readonly List<LogicTransitionData> Transitions;

        public Term GetOrAddTerm(string value)
        {
            if (value == null) throw new ArgumentNullException(nameof(value));

            if (termLookup.TryGetValue(value, out Term t)) return t;
            t = new(terms.Count, value);
            terms.Add(t);
            return termLookup[value] = t;
        }

        public void AddItem(LogicItem item)
        {
            PrefabItems[item.Name] = item;
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
                        GetOrAddTerm(def.name);
                        LogicLookup[def.name] = LP.ParseInfixToClause(def.logic);
                        Waypoints.Add(def.name);
                    }
                    break;

                case JsonType.Transitions:
                    foreach (RawLogicTransition def in JsonUtil.Deserialize<RawLogicTransition[]>(jtr) ?? Enumerable.Empty<RawLogicTransition>())
                    {
                        GetOrAddTerm(def.Name);
                        LogicLookup[def.Name] = LP.ParseInfixToClause(def.logic);
                        Transitions.Add(def.GetTransitionData());
                    }
                    break;

                case JsonType.Macros:
                    LP.SetMacro(JsonUtil.Deserialize<Dictionary<string, string>>(jtr));
                    break;

                case JsonType.Items:
                    {
                        foreach (JObject jo in JArray.Load(jtr).Cast<JObject>())
                        {
                            UnparsedItems[jo.Value<string>("Name")] = jo;
                        }
                    }
                    break;

                case JsonType.Locations:
                    foreach (RawLogicDef def in JsonUtil.Deserialize<RawLogicDef[]>(jtr) ?? Enumerable.Empty<RawLogicDef>())
                    {
                        LogicLookup[def.name] = LP.ParseInfixToClause(def.logic);
                    }
                    break;

                case JsonType.LogicEdit:
                    foreach (RawLogicDef def in JsonUtil.Deserialize<RawLogicDef[]>(jtr) ?? Enumerable.Empty<RawLogicDef>())
                    {
                        LogicClauseBuilder lcb = LP.ParseInfixToBuilder(def.logic);
                        if (lcb.Tokens.Any(lt => lt is SimpleToken st && st.Name == "ORIG"))
                        {
                            lcb.Subst(LP.GetTermToken("ORIG"), LogicLookup[def.name]);
                        }
                        LogicLookup[def.name] = new(lcb);
                    }
                    break;
                case JsonType.MacroEdit:
                    foreach (KeyValuePair<string, string> kvp in JsonUtil.Deserialize<Dictionary<string, string>>(jtr) ?? Enumerable.Empty<KeyValuePair<string, string>>())
                    {
                        LogicClauseBuilder lcb = LP.ParseInfixToBuilder(kvp.Value);
                        if (lcb.Tokens.Any(lt => lt is SimpleToken st && st.Name == "ORIG"))
                        {
                            lcb.Subst(LP.GetTermToken("ORIG"), LP.GetMacro(kvp.Key));
                        }
                        LP.SetMacro(kvp.Key, new LogicClause(lcb));
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
                        GetOrAddTerm(def.name);
                        LogicLookup[def.name] = LP.ParseInfixToClause(def.logic);
                        Waypoints.Add(def.name);
                    }
                    break;

                case JsonType.Transitions:
                    foreach (RawLogicTransition def in t.ToObject<List<RawLogicTransition>>() ?? Enumerable.Empty<RawLogicTransition>())
                    {
                        GetOrAddTerm(def.Name);
                        LogicLookup[def.Name] = LP.ParseInfixToClause(def.logic);
                        Transitions.Add(def.GetTransitionData());
                    }
                    break;

                case JsonType.Macros:
                    LP.SetMacro(t.ToObject<Dictionary<string, string>>());
                    break;

                case JsonType.Items:
                    {
                        foreach (JObject jo in (JArray)t)
                        {
                            UnparsedItems[jo.Value<string>("Name")] = jo;
                        }
                    }
                    break;

                case JsonType.Locations:
                    foreach (RawLogicDef def in t.ToObject<List<RawLogicDef>>() ?? Enumerable.Empty<RawLogicDef>())
                    {
                        LogicLookup[def.name] = LP.ParseInfixToClause(def.logic);
                    }
                    break;

                case JsonType.LogicEdit:
                    foreach (RawLogicDef def in t.ToObject<RawLogicDef[]>() ?? Enumerable.Empty<RawLogicDef>())
                    {
                        LogicClauseBuilder lcb = LP.ParseInfixToBuilder(def.logic);
                        if (lcb.Tokens.Any(lt => lt is SimpleToken st && st.Name == "ORIG"))
                        {
                            lcb.Subst(LP.GetTermToken("ORIG"), LogicLookup[def.name]);
                        }
                        LogicLookup[def.name] = new(lcb);
                    }
                    break;
                case JsonType.MacroEdit:
                    foreach (KeyValuePair<string, string> kvp in t.ToObject<Dictionary<string, string>>() ?? Enumerable.Empty<KeyValuePair<string, string>>())
                    {
                        LogicClauseBuilder lcb = LP.ParseInfixToBuilder(kvp.Value);
                        if (lcb.Tokens.Any(lt => lt is SimpleToken st && st.Name == "ORIG"))
                        {
                            lcb.Subst(LP.GetTermToken("ORIG"), LP.GetMacro(kvp.Key));
                        }
                        LP.SetMacro(kvp.Key, new LogicClause(lcb));
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

using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RandomizerCore.Json;
using RandomizerCore.Logic.StateLogic;
using RandomizerCore.LogicItems;
using RandomizerCore.LogicItems.Templates;
using RandomizerCore.StringLogic;

namespace RandomizerCore.Logic
{
    public class LogicManagerBuilder
    {
        public LogicManagerBuilder()
        {
            Terms = new();
            LP = new();
            VariableResolver = new();
            ItemLookup = new();
            LogicLookup = new();
            Waypoints = new();
            Transitions = new();
            StateManager = new();
        }

        public LogicManagerBuilder(LogicManagerBuilder source)
        {
            Terms = new(source.Terms);
            LP = new(source.LP);
            VariableResolver = source.VariableResolver;
            ItemLookup = new(source.ItemLookup);
            LogicLookup = new(source.LogicLookup);
            Waypoints = new(source.Waypoints);
            Transitions = new(source.Transitions);
            StateManager = new(source.StateManager);
        }

        public LogicManagerBuilder(LogicManager source)
        {
            Terms = new(source.Terms);
            LP = new(source.LP);
            VariableResolver = source.VariableResolver;
            ItemLookup = source.ItemLookup.ToDictionary(kvp => kvp.Key, kvp => (ILogicItemTemplate)kvp.Value);
            LogicLookup = new();
            foreach (KeyValuePair<string, LogicDef> kvp in source.LogicLookup) AddLogicDef(new(kvp.Key, kvp.Value.InfixSource));
            Waypoints = new(source.Waypoints.Select(w => w.Name));
            Transitions = new(source.TransitionLookup.Values.Select(lt => lt.Name));
            StateManager = new(source.StateManager);
        }


        public readonly TermCollectionBuilder Terms;
        public LogicProcessor LP { get; set; }
        public VariableResolver VariableResolver { get; set; }
        public readonly Dictionary<string, ILogicItemTemplate> ItemLookup;
        public readonly Dictionary<string, LogicClause> LogicLookup;
        public readonly HashSet<string> Waypoints;
        public readonly HashSet<string> Transitions;
        public readonly StateManagerBuilder StateManager;

        /// <summary>
        /// Returns whether the string is a key in the term lookup.
        /// </summary>
        public bool IsTerm(string value)
        {
            return Terms.IsTerm(value);
        }

        /// <summary>
        /// If the string is a key in the term lookup, returns the corresponding term. Otherwise, creates, saves, and returns a new term, of Byte type.
        /// </summary>
        public Term GetOrAddTerm(string value)
        {
            return Terms.GetOrAddTerm(value, TermType.SignedByte);
        }

        /// <summary>
        /// If the string is a key in the term lookup, returns the corresponding term. Otherwise, creates, saves, and returns a new term of the given type.
        /// </summary>
        /// <exception cref="InvalidOperationException">The term has been defined with a different type.</exception>
        public Term GetOrAddTerm(string value, TermType type)
        {
            return Terms.GetOrAddTerm(value, type);
        }

        /// <summary>
        /// Adds the item template to the builder's dictionary. Overwrites any existing item with the same name.
        /// </summary>
        /// <param name="item"></param>
        public void AddItem(ILogicItemTemplate item)
        {
            ItemLookup[item.Name] = item;
        }

        /// <summary>
        /// Adds the RawLogicDef as a new waypoint. Overwrites any existing logic with the same name.
        /// </summary>
        public void AddWaypoint(RawWaypointDef def)
        {
            if (!IsTerm(def.name))
            {
                GetOrAddTerm(def.name, def.stateless ? TermType.SignedByte : TermType.State);
            }
            try
            {
                LogicLookup[def.name] = LP.ParseInfixToClause(def.logic);
            }
            catch (Exception e)
            {
                throw new InvalidOperationException($"Logic \"{def.logic}\" for {def.name} is malformed.", e);
            }
            Waypoints.Add(def.name);
        }

        /// <summary>
        /// Adds the RawLogicTransition as a new transition. Overwrites any existing logic with the same name.
        /// </summary>
        public void AddTransition(RawLogicDef def)
        {
            if (!IsTerm(def.name))
            {
                GetOrAddTerm(def.name, TermType.State);
            }
            try
            {
                LogicLookup[def.name] = LP.ParseInfixToClause(def.logic);
            }
            catch (Exception e)
            {
                throw new InvalidOperationException($"Logic \"{def.logic}\" for {def.name} is malformed.", e);
            }
            Transitions.Add(def.name);
        }

        /// <summary>
        /// Adds the RawLogicDef for general use. Overwrites any existing logic with the same name.
        /// </summary>
        public void AddLogicDef(RawLogicDef def)
        {
            try
            {
                LogicLookup[def.name] = LP.ParseInfixToClause(def.logic);
            }
            catch (Exception e)
            {
                throw new InvalidOperationException($"Logic \"{def.logic}\" for {def.name} is malformed.", e);
            }
        }

        /// <summary>
        /// If the input contains the ORIG token and the logic def is already defined, substitutes the old value for ORIG in the input, and overwrites the old logic.
        /// <br/>If the input does not contain the ORIG token, is equivalent to AddLogicDef.
        /// </summary>
        public void DoLogicEdit(RawLogicDef def)
        {
            LogicClauseBuilder lcb;
            try
            {
                lcb = LP.ParseInfixToBuilder(def.logic);
            }
            catch (Exception e)
            {
                throw new InvalidOperationException($"Logic edit \"{def.logic}\" for {def.name} is malformed.", e);
            }

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
            LogicClauseBuilder lcb;
            try
            {
                lcb = LP.ParseInfixToBuilder(kvp.Value);
            }
            catch (Exception e)
            {
                throw new InvalidOperationException($"Logic edit \"{kvp.Value}\" for macro {kvp.Key} is malformed.", e);
            }
            
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
            LogicClause lc;
            try
            {
                lc = LP.ParseInfixToClause(def.replacement);
            }
            catch (Exception e)
            {
                throw new InvalidOperationException($"Logic subst replacement \"{def.replacement}\" for {def.old} in {def.name} is malformed.", e);
            }
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
            StateData,
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
                    JToken termDoc = JToken.Load(jtr); // we need to JToken to identify which format the term doc is in
                    DeserializeJson(type, termDoc);
                    break;

                case JsonType.Waypoints:
                    foreach (RawWaypointDef def in JsonUtil.Deserialize<RawWaypointDef[]>(jtr) ?? Enumerable.Empty<RawWaypointDef>())
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
                            AddItem(new JsonItemTemplate(jo));
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
                        AddItem(template);
                    }
                    break;
                case JsonType.StateData:
                    RawStateData rsd = JsonUtil.Deserialize<RawStateData>(jtr);
                    StateManager.AppendRawStateData(rsd);
                    break;
            }
        }

        public void DeserializeJson(JsonType type, JToken t)
        {
            switch (type)
            {
                case JsonType.Terms:
                    if (t.Type == JTokenType.Array)
                    {
                        foreach (string term in t.ToObject<List<string>>())
                        {
                            GetOrAddTerm(term, TermType.Int); // legacy format
                        }
                    }
                    else
                    {
                        foreach (KeyValuePair<string, List<string>> kvp in t.ToObject<Dictionary<string, List<string>>>())
                        {
                            if (!Enum.TryParse(kvp.Key, true, out TermType termType))
                            {
                                Log($"Unable to parse {kvp.Key} as TermType, assuming Int...");
                                termType = TermType.Int;
                            }
                            foreach (string term in kvp.Value)
                            {
                                GetOrAddTerm(term, termType);
                            }
                        }
                    }
                    break;

                case JsonType.Waypoints:
                    foreach (RawWaypointDef def in t?.ToObject<List<RawWaypointDef>>() ?? Enumerable.Empty<RawWaypointDef>())
                    {
                        AddWaypoint(def);
                    }
                    break;

                case JsonType.Transitions:
                    foreach (RawLogicDef def in t?.ToObject<List<RawLogicDef>>() ?? Enumerable.Empty<RawLogicDef>())
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
                            AddItem(new JsonItemTemplate(jo));
                        }
                    }
                    break;

                case JsonType.Locations:
                    foreach (RawLogicDef def in t?.ToObject<List<RawLogicDef>>() ?? Enumerable.Empty<RawLogicDef>())
                    {
                        AddLogicDef(def);
                    }
                    break;

                case JsonType.LogicEdit:
                    foreach (RawLogicDef def in t?.ToObject<RawLogicDef[]>() ?? Enumerable.Empty<RawLogicDef>())
                    {
                        DoLogicEdit(def);
                    }
                    break;
                case JsonType.MacroEdit:
                    foreach (KeyValuePair<string, string> kvp in t?.ToObject<Dictionary<string, string>>() ?? Enumerable.Empty<KeyValuePair<string, string>>())
                    {
                        DoMacroEdit(kvp);
                    }
                    break;
                case JsonType.LogicSubst:
                    foreach (RawSubstDef def in t?.ToObject<List<RawSubstDef>>() ?? Enumerable.Empty<RawSubstDef>())
                    {
                        DoSubst(def);
                    }
                    break;
                case JsonType.ItemTemplates:
                    foreach (ILogicItemTemplate template in t?.ToObject<IEnumerable<ILogicItemTemplate>>() ?? Enumerable.Empty<ILogicItemTemplate>())
                    {
                        AddItem(template);
                    }
                    break;
                case JsonType.StateData:
                    RawStateData rsd = t?.ToObject<RawStateData>();
                    StateManager.AppendRawStateData(rsd);
                    break;
            }
        }

        public void DeserializeFile(JsonType type, ILogicFormat logicFormat, Stream s)

        {
            switch(type)
            {
                case JsonType.Terms:
                    foreach ((string term, TermType termType) in logicFormat.LoadTerms(s))
                    {
                        GetOrAddTerm(term, termType);
                    }
                    break;

                case JsonType.Waypoints:
                    foreach (RawWaypointDef def in logicFormat.LoadWaypoints(s))
                    {
                        AddWaypoint(def);
                    }
                    break;

                case JsonType.Transitions:
                    foreach (RawLogicDef def in logicFormat.LoadTransitions(s))
                    {
                        AddTransition(def);
                    }
                    break;

                case JsonType.Macros:
                    LP.SetMacro(logicFormat.LoadMacros(s));
                    break;

                case JsonType.Items:
                    foreach (object obj in logicFormat.LoadItems(s))
                    {
                        if (obj is LogicItem item)
                        {
                            AddItem(item);
                        }
                        else if (obj is ILogicItemTemplate templ)
                        {
                            AddItem(templ);
                        }
                        else
                        {
                            throw new FormatException($"Unexpected item of type {obj.GetType()} " +
                                $"returned by logic format of type {logicFormat.GetType()}");
                        }
                    }
                    break;

                case JsonType.Locations:
                    foreach (RawLogicDef def in logicFormat.LoadLocations(s))
                    {
                        AddLogicDef(def);
                    }
                    break;

                case JsonType.LogicEdit:
                    foreach (RawLogicDef def in logicFormat.LoadLogicEdits(s))
                    {
                        DoLogicEdit(def);
                    }
                    break;
                case JsonType.MacroEdit:
                    foreach (KeyValuePair<string, string> kvp in logicFormat.LoadMacroEdits(s))
                    {
                        DoMacroEdit(kvp);
                    }
                    break;

                case JsonType.LogicSubst:
                    foreach (RawSubstDef def in logicFormat.LoadLogicSubstitutions(s))
                    {
                        DoSubst(def);
                    }
                    break;
                case JsonType.ItemTemplates:
                    foreach (ILogicItemTemplate template in logicFormat.LoadItemTemplates(s))
                    {
                        AddItem(template);
                    }
                    break;
                case JsonType.StateData:
                    RawStateData rsd = logicFormat.LoadStateData(s);
                    StateManager.AppendRawStateData(rsd);
                    break;
            }
        }
        public Term GetTerm(string term)
        {
            return Terms.TermLookup[term];
        }

        public Term GetTerm(int id)
        {
            return Terms[id];
        }
    }
}

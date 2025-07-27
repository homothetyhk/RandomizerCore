using RandomizerCore.Logic.StateLogic;
using RandomizerCore.LogicItems;
using RandomizerCore.StringItems;
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
        }


        public readonly TermCollectionBuilder Terms;
        public LogicProcessor LP { get; set; }
        public VariableResolver VariableResolver { get; set; }
        public readonly Dictionary<string, ILogicItemTemplate> ItemLookup;
        public readonly Dictionary<string, LogicClause> LogicLookup;
        public readonly HashSet<string> Waypoints;
        public readonly HashSet<string> Transitions;

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

        public void AddItem(LogicItem item)
        {
            AddItem((ILogicItemTemplate)item);
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

            bool exists = LogicLookup.TryGetValue(def.name, out LogicClause orig);
            lcb.PartialCoalesce(tt => tt is SimpleToken { Name: "ORIG" } ? exists : null);
            if (lcb.Tokens.Any(lt => lt is SimpleToken st && st.Name == "ORIG"))
            {
                if (!exists)
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

            bool exists = LP.IsMacro(kvp.Key);
            lcb.PartialCoalesce(tt => tt is SimpleToken { Name: "ORIG" } ? exists : null);
            if (lcb.Tokens.Any(lt => lt is SimpleToken st && st.Name == "ORIG"))
            {
                if (!exists)
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

        public void DeserializeFile(LogicFileType type, ILogicFormat logicFormat, Stream s)
        {
            switch(type)
            {
                case LogicFileType.Terms:
                    foreach ((string term, TermType termType) in logicFormat.LoadTerms(s))
                    {
                        GetOrAddTerm(term, termType);
                    }
                    break;

                case LogicFileType.Waypoints:
                    foreach (RawWaypointDef def in logicFormat.LoadWaypoints(s))
                    {
                        AddWaypoint(def);
                    }
                    break;

                case LogicFileType.Transitions:
                    foreach (RawLogicDef def in logicFormat.LoadTransitions(s))
                    {
                        AddTransition(def);
                    }
                    break;

                case LogicFileType.Macros:
                    LP.SetMacro(logicFormat.LoadMacros(s));
                    break;

                case LogicFileType.Locations:
                    foreach (RawLogicDef def in logicFormat.LoadLocations(s))
                    {
                        AddLogicDef(def);
                    }
                    break;

                case LogicFileType.LogicEdit:
                    foreach (RawLogicDef def in logicFormat.LoadLogicEdits(s))
                    {
                        DoLogicEdit(def);
                    }
                    break;
                case LogicFileType.MacroEdit:
                    foreach (KeyValuePair<string, string> kvp in logicFormat.LoadMacroEdits(s))
                    {
                        DoMacroEdit(kvp);
                    }
                    break;

                case LogicFileType.LogicSubst:
                    foreach (RawSubstDef def in logicFormat.LoadLogicSubstitutions(s))
                    {
                        DoSubst(def);
                    }
                    break;
                case LogicFileType.Items:
                    foreach (ILogicItemTemplate template in logicFormat.LoadItems(s))
                    {
                        AddItem(template);
                    }
                    break;

                case LogicFileType.ItemTemplates:
                    foreach (ILogicItemTemplate template in logicFormat.LoadItemTemplates(s))
                    {
                        AddItem(template);
                    }
                    break;

                case LogicFileType.ItemStrings:
                    foreach (ILogicItemTemplate template in logicFormat.LoadItemStrings(s))
                    {
                        AddItem(template);
                    }
                    break;
            }
        }

        /// <summary>
        /// Applies the data to the LMB. The type of the data should match the output of ILogicFormat for the given LogicFileType.
        /// </summary>
        public void LoadData(LogicFileType type, object? data)
        {
            if (data is null) return;

            switch (type)
            {
                case LogicFileType.Terms:
                    foreach ((string term, TermType termType) in (IEnumerable<(string, TermType)>)data)
                    {
                        GetOrAddTerm(term, termType);
                    }
                    break;

                case LogicFileType.Waypoints:
                    foreach (RawWaypointDef def in (IEnumerable<RawWaypointDef>)data)
                    {
                        AddWaypoint(def);
                    }
                    break;

                case LogicFileType.Transitions:
                    foreach (RawLogicDef def in (IEnumerable<RawLogicDef>)data)
                    {
                        AddTransition(def);
                    }
                    break;

                case LogicFileType.Macros:
                    LP.SetMacro((Dictionary<string, string>)data);
                    break;

                case LogicFileType.Locations:
                    foreach (RawLogicDef def in (IEnumerable<RawLogicDef>)data)
                    {
                        AddLogicDef(def);
                    }
                    break;

                case LogicFileType.LogicEdit:
                    foreach (RawLogicDef def in (IEnumerable<RawLogicDef>)data)
                    {
                        DoLogicEdit(def);
                    }
                    break;
                case LogicFileType.MacroEdit:
                    foreach (KeyValuePair<string, string> kvp in (IEnumerable<KeyValuePair<string, string>>)data)
                    {
                        DoMacroEdit(kvp);
                    }
                    break;

                case LogicFileType.LogicSubst:
                    foreach (RawSubstDef def in (IEnumerable<RawSubstDef>)data)
                    {
                        DoSubst(def);
                    }
                    break;
                case LogicFileType.Items:
                    foreach (ILogicItemTemplate template in (IEnumerable<ILogicItemTemplate>)data)
                    {
                        AddItem(template);
                    }
                    break;

                case LogicFileType.ItemTemplates:
                    foreach (ILogicItemTemplate template in (IEnumerable<ILogicItemTemplate>)data)
                    {
                        AddItem(template);
                    }
                    break;

                case LogicFileType.ItemStrings:
                    foreach (ILogicItemTemplate template in (IEnumerable<StringItemTemplate>)data)
                    {
                        AddItem(template);
                    }
                    break;

                case LogicFileType.StateData:;
                    throw new NotSupportedException("State data can no longer be loaded into the LogicManagerBuilder.");
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

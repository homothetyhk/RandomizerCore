using Newtonsoft.Json;
using RandomizerCore.Exceptions;
using RandomizerCore.Json;
using RandomizerCore.Logic.StateLogic;
using RandomizerCore.LogicItems;
using RandomizerCore.StringItems;
using RandomizerCore.StringLogic;
using System.Collections.ObjectModel;

namespace RandomizerCore.Logic
{
    [JsonConverter(typeof(LMConverter))]
    public class LogicManager
    {
        public readonly TermCollection Terms;
        public readonly ReadOnlyDictionary<string, LogicDef> LogicLookup;
        public readonly ReadOnlyDictionary<string, LogicItem> ItemLookup;
        public readonly ReadOnlyDictionary<string, LogicTransition> TransitionLookup;
        public readonly ReadOnlyCollection<LogicWaypoint> Waypoints;
        public readonly ReadOnlyCollection<LogicVariable> Variables;
        public readonly StateManager StateManager;

        // Data structures dynamically constructed to correspond to logic
        private readonly Dictionary<string, LogicDef> _logicDefs;
        public LogicProcessor LP { get; }
        private readonly List<LogicVariable> _variables;
        private readonly Dictionary<string, int> _variableIndices;
        private readonly Dictionary<string, LogicItem> _items;
        private readonly Dictionary<string, LogicTransition> _transitions;

        private readonly LogicManagerBuilder? _source; // set to null on exiting constructor

        internal readonly CycleDetector _cycleDetector;

        private readonly StringItemBuilder _stringItemBuilder;
        private readonly DNFLogicDefBuilder _dnfLogicBuilder;

        public VariableResolver VariableResolver { get; }

        public const int intVariableOffset = -100;

        public LogicManager(LogicManagerBuilder source)
        {
            _source = source;
            LP = source.LP;
            VariableResolver = source.VariableResolver;
            StateManager = new(source.StateManager);
            _cycleDetector = new();
            _stringItemBuilder = new(this);
            _dnfLogicBuilder = new(this);

            List<Exception> errors = [];

            // Terms
            Terms = new(source.Terms);

            // Variables
            VariableResolver = source.VariableResolver ?? new VariableResolver();
            _variables = new();
            _variableIndices = new();
            Variables = new(_variables);

            // Logic
            _logicDefs = new(source.LogicLookup.Count);
            foreach (KeyValuePair<string, LogicClause> kvp in source.LogicLookup)
            {
                if (!_logicDefs.ContainsKey(kvp.Key)) // logic may be created out of sequence due to references
                {
                    try
                    {
                        _logicDefs.Add(kvp.Key, CreateDNFLogicDef(kvp.Key, kvp.Value));
                    }
                    catch (Exception e)
                    {
                        errors.Add(e);
                        _cycleDetector.Reset();
                        _dnfLogicBuilder.Reset();
                    }
                }
            }
            if (errors.Count > 0) throw errors.Count == 1 ? errors[0] : new AggregateException(errors);
            LogicLookup = new(_logicDefs);


            // Waypoints
            Waypoints = new(source.Waypoints.Select(name => new LogicWaypoint(Terms.GetTerm(name)!, _logicDefs[name] as StateLogicDef ?? CreateDNFLogicDef(new(name, _logicDefs[name].InfixSource)))).ToArray());
            
            // Transitions
            _transitions = source.Transitions.ToDictionary(name => name, name => new LogicTransition(_logicDefs[name] as StateLogicDef ?? CreateDNFLogicDef(new(name, _logicDefs[name].InfixSource)), Terms.GetTerm(name)!));
            TransitionLookup = new(_transitions);

            // Items
            _items = new(source.ItemLookup.Count);
            JsonSerializer js = JsonUtil.GetLogicSerializer(this);
            foreach (var kvp in source.ItemLookup)
            {
                if (!_items.ContainsKey(kvp.Key)) // items may be created out of sequence due to references
                {
                    try
                    {
                        _items.Add(kvp.Key, kvp.Value.Create(this));
                    }
                    catch (Exception e)
                    {
                        errors.Add(e);
                        _cycleDetector.Reset();
                        _dnfLogicBuilder.Reset();
                    }
                }
            }
            if (errors.Count > 0) throw errors.Count == 1 ? errors[0] : new AggregateException(errors);
            ItemLookup = new(_items);

            _source = null;
            _dnfLogicBuilder.Trim();
        }

        /// <summary>
        /// Fetches the logic def by name. Returns null if not defined.
        /// </summary>
        public LogicDef? GetLogicDef(string name)
        {
            if (!_logicDefs.TryGetValue(name, out LogicDef def))
            {
                return null;
            }

            return def;
        }

        /// <summary>
        /// Fetches the logic def by name.
        /// </summary>
        /// <exception cref="ArgumentException">The logic def is not defined.</exception>
        public LogicDef GetLogicDefStrict(string name)
        {
            return GetLogicDef(name) ?? throw new ArgumentException($"LogicDef {name} is not defined.");
        }

        /// <summary>
        /// Fetches the term by name. Returns null if not defined.
        /// </summary>
        public Term? GetTerm(string item)
        {
            if (Terms.GetTerm(item) is not Term t)
            {
                return null;
            }

            return t;
        }

        /// <summary>
        /// Fetches the term by name.
        /// </summary>
        /// <exception cref="ArgumentException">The tern is not defined.</exception>
        public Term GetTermStrict(string item)
        {
            return Terms.GetTerm(item) ?? throw new ArgumentException($"Term {item} is not defined.");
        }

        /// <summary>
        /// Fetches the term by its id.
        /// </summary>
        public Term GetTerm(int id)
        {
            return Terms[id];
        }

        /// <summary>
        /// Fetches the variable by its id.
        /// </summary>
        public LogicVariable GetVariable(int id)
        {
            return _variables[intVariableOffset - id];
        }

        /// <summary>
        /// Fetches the variable by name. If the variable has not yet been instantiated, caches it and gives it an id. Returns null if the name cannot be resolved to a variable.
        /// </summary>
        public LogicVariable? GetVariable(string name)
        {
            if (GetVariableID(name) is int id) return GetVariable(id);
            else return null;
        }

        /// <summary>
        /// Fetches the variable by name. If the variable has not yet been instantiated, caches it and gives it an id.
        /// </summary>
        /// <exception cref="ArgumentException">The name cannot be resolved to a variable.</exception>
        public LogicVariable GetVariableStrict(string name)
        {
            if (GetVariableID(name) is int id) return GetVariable(id);
            throw new ArgumentException($"Unable to resolve {name} to LogicVariable.");
        }

        internal int? GetVariableID(string name)
        {
            if (_variableIndices.TryGetValue(name, out int index)) return index;

            LogicVariable lv;
            try
            {
                if (!VariableResolver.TryMatch(this, name, out lv)) return null;
                if (lv is null) throw new NullReferenceException($"Parsed {name} to null LogicVariable!");
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error parsing {name} to LogicVariable.", e);
            }

            index = intVariableOffset - _variables.Count;
            _variableIndices.Add(name, index);
            _variables.Add(lv);
            return index;
        }

        public bool IsTermOrVariable(string name)
        {
            return name != null && (Terms.IsTerm(name) || _variableIndices.ContainsKey(name) || VariableResolver.CanMatch(this, name));
        }

        /// <summary>
        /// Fetches the logic item by name. Returns null if not defined.
        /// </summary>
        public LogicItem? GetItem(string name)
        {
            if (!_items.TryGetValue(name, out LogicItem item))
            {
                return null;
            }

            return item;
        }

        /// <summary>
        /// Fetches the logic item by name.
        /// </summary>
        /// <exception cref="ArgumentException">The logic item is not defined.</exception>
        public LogicItem GetItemStrict(string name)
        {
            return GetItem(name) ?? throw new ArgumentException($"LogicItem {name} is not defined.");
        }

        /// <summary>
        /// Fetches the logic transition by name. Returns null if not defined.
        /// </summary>
        public LogicTransition? GetTransition(string name)
        {
            if (!_transitions.TryGetValue(name, out LogicTransition transition))
            {
                return null;
            }

            return transition;
        }

        /// <summary>
        /// Fetches the logic transition by name.
        /// </summary>
        /// <exception cref="ArgumentException">The logic transition is not defined.</exception>
        public LogicTransition GetTransitionStrict(string name)
        {
            return GetTransition(name) ?? throw new ArgumentException($"LogicTransition {name} is not defined.");
        }

        public LogicDef FromString(RawLogicDef def)
        {
            return CreateDNFLogicDef(def);
        }

        /// <summary>
        /// Parses infix logic which should take progression data and a single state as input.
        /// </summary>
        public SingleStateLogic CreateSingleStateLogic(RawLogicDef def)
        {
            return CreateSingleStateLogic(def.name, new LogicClause(def.logic));
        }

        public SingleStateLogic CreateSingleStateLogic(string name, LogicClause c)
        {
            try
            {
                RPNLogicDefBuilder rlb = new(this, name, RPNLogicDefBuilder.AllowedVariableTypes.LogicInt | RPNLogicDefBuilder.AllowedVariableTypes.StateAccessVariable);
                rlb.Process(c);
                rlb.MoveComparisonOperatorsAfterArgs(); // SSRPNL is read backwards, so comparison ops should be after their args
                return new SingleStateRPNLogic(name, rlb.GetRawLogic(), this, c.ToInfix());
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error in processing logic for {name}.", e);
            }
        }

        public RPNLogicDef CreateRPNLogicDef(RawLogicDef def)
        {
            try
            {
                return CreateRPNLogicDef(def.name, def.logic, LP.ParseInfixToList(def.logic));
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error in processing logic for {def.name}.", e);
            }
        }
        [Obsolete]
        public RPNLogicDef FromTokens(string name, LogicClause c) => CreateRPNLogicDef(name, c.ToInfix(), c);
        private RPNLogicDef CreateRPNLogicDef(string name, string infix, IEnumerable<LogicToken> tokens)
        {
            try
            {
                RPNLogicDefBuilder rlb = new(this, name, RPNLogicDefBuilder.AllowedVariableTypes.LogicInt);
                rlb.Process(tokens);
                return new(name, rlb.GetRawLogic(), this, infix);
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error in processing logic for {name}.", e);
            }
        }

        public DNFLogicDef CreateDNFLogicDef(RawLogicDef def)
        {
            try
            {
                return _dnfLogicBuilder.CreateDNFLogicDef(def.name, def.logic, LP.ParseInfixToList(def.logic));
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error in processing logic for {def.name}.", e);
            }
        }
        public DNFLogicDef CreateDNFLogicDef(string name, LogicClause tokens) => _dnfLogicBuilder.CreateDNFLogicDef(name, tokens.ToInfix(), tokens.Tokens.ToList());

        public LogicItem FromItemString(string name, string itemDef)
        {
            if (string.IsNullOrWhiteSpace(itemDef))
            {
                return new StringItem(name, itemDef, EmptyEffect.Instance);
            }

            return new StringItem(name, itemDef, _stringItemBuilder.ParseStringToEffect(name, itemDef));
        }

        internal LogicDef ResolveLogicDefReference(string source, string reference)
        {
            if (_logicDefs.TryGetValue(reference, out LogicDef value))
            {
                return value;
            }

            if (_source is null || !_source.LogicLookup.TryGetValue(reference, out LogicClause c))
            {
                throw MissingLogicReferenceError(source, reference);
            }

            _cycleDetector.PushLogic(reference);
            value = CreateDNFLogicDef(reference, c);
            _logicDefs.Add(reference, value);
            _cycleDetector.PopLogic(reference);

            return value;
        }

        internal LogicItem ResolveItemReference(string source, string reference)
        {
            if (_items.TryGetValue(reference, out LogicItem item))
            {
                return item;
            }

            if (_source is null || !_source.ItemLookup.TryGetValue(reference, out ILogicItemTemplate t))
            {
                throw MissingItemReferenceError(source, reference);
            }

            _cycleDetector.PushItem(reference);
            item = t.Create(this);
            _items.Add(reference, item);
            _cycleDetector.PopItem(reference);

            return item;
        }

        private Exception MissingLogicReferenceError(string name, string reference)
        {
            return new ArgumentException($"Failed to parse LogicDef {name}: unknown reference to LogicDef {reference}.");
        }

        private Exception MissingItemReferenceError(string name, string reference)
        {
            return new ArgumentException($"Failed to parse item {name}: unknown reference to item {reference}.");
        }

        internal class CycleDetector
        {
            private readonly HashSet<string> itemCycleDetector = [];
            private readonly HashSet<string> logicCycleDetector = [];

            public void PushItem(string itemName)
            {
                if (!itemCycleDetector.Add(itemName)) throw CircularItemReferenceError(itemName);
            }

            public void PopItem(string itemName)
            {
                itemCycleDetector.Remove(itemName);
            }

            public void PushLogic(string logicName)
            {
                if (!logicCycleDetector.Add(logicName)) throw CircularLogicReferenceError(logicName);
            }

            public void PopLogic(string logicName)
            {
                logicCycleDetector.Remove(logicName);
            }

            public void Reset()
            {
                itemCycleDetector.Clear();
                logicCycleDetector.Clear();
            }

            private Exception CircularItemReferenceError(string reference)
            {
                string msg = $"Circular item reference detected:\n{string.Join(" -> ", itemCycleDetector.Append(reference))}";
                return new ReferenceCycleException(msg);
            }

            private Exception CircularLogicReferenceError(string reference)
            {
                string msg = $"Circular logic reference detected:\n{string.Join(" -> ", logicCycleDetector.Append(reference))}";
                return new ReferenceCycleException(msg);
            }
        }
    }
}

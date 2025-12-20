using RandomizerCore.Exceptions;
using RandomizerCore.Logic.StateLogic;
using RandomizerCore.LogicItems;
using RandomizerCore.StringItems;
using RandomizerCore.StringLogic;
using RandomizerCore.StringParsing;
using System.Collections.ObjectModel;

namespace RandomizerCore.Logic
{
    public class LogicManager
    {
        public readonly TermCollection Terms;
        public readonly ReadOnlyDictionary<string, LogicDef> LogicLookup;
        public readonly ReadOnlyDictionary<string, LogicItem> ItemLookup;
        public readonly ReadOnlyDictionary<string, LogicTransition> TransitionLookup;
        public readonly ReadOnlyCollection<LogicWaypoint> Waypoints;
        public readonly ReadOnlyCollection<LogicVariable> Variables;
        public readonly ReadOnlyDictionary<string, MacroDef> MacroLookup;
        public readonly StateManager StateManager;

        // Data structures dynamically constructed to correspond to logic
        private readonly Dictionary<string, LogicDef> _logicDefs;
        [Obsolete]
        public LogicProcessor LP { get; }
        private readonly List<LogicVariable> _variables;
        private readonly Dictionary<string, int> _variableIndices;
        private readonly Dictionary<string, LogicItem> _items;
        private readonly Dictionary<string, LogicTransition> _transitions;
        private readonly Dictionary<string, MacroDef> _macros;

        private readonly LogicManagerBuilder? _source; // set to null on exiting constructor

        internal readonly DNFConverter _dNFConverter;
        private readonly CycleDetector _cycleDetector;


        public VariableResolver VariableResolver { get; }

        public const int intVariableOffset = -100;

        public LogicManager(LogicManagerBuilder source)
        {
            _source = source;
            VariableResolver = source.VariableResolver;
            StateManager = new(VariableResolver.GetStateModel());
            _cycleDetector = new();
            _dNFConverter = new();

            List<Exception> errors = [];

            // Terms
            Terms = new(source.Terms);

            // Variables
            VariableResolver = source.VariableResolver ?? new VariableResolver();
            _variables = new();
            _variableIndices = new();
            Variables = new(_variables);

            // Macros
#pragma warning disable CS0612 // Type or member is obsolete
            LP = new(source.LP);
#pragma warning restore CS0612 // Type or member is obsolete
            _macros = [];
            foreach (var kvp in source.MacroLookup)
            {
                try
                {
                    _ = GetMacroStrict(kvp.Key);
                }
                catch (Exception e)
                {
                    errors.Add(e);
                    _cycleDetector.Clear();
                }
            }
            MacroLookup = new(_macros);

            // Logic
            _logicDefs = new(source.LogicLookup.Count);
            foreach (KeyValuePair<string, LogicClause> kvp in source.LogicLookup)
            {
                try
                {
                    _ = GetLogicDefStrict(kvp.Key);
                }
                catch (Exception e)
                {
                    errors.Add(e);
                    _cycleDetector.Clear();
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
            foreach (var kvp in source.ItemLookup)
            {
                try
                {
                    _ = GetItemStrict(kvp.Key);
                }
                catch (Exception e)
                {
                    errors.Add(e);
                    _cycleDetector.Clear();
                }
            }
            if (errors.Count > 0) throw errors.Count == 1 ? errors[0] : new AggregateException(errors);
            ItemLookup = new(_items);

            _source = null;
            _dNFConverter.Trim();
        }

        /// <summary>
        /// Fetches the logic def by name. Returns null if not defined.
        /// </summary>
        public LogicDef? GetLogicDef(string name)
        {
            if (_logicDefs.TryGetValue(name, out LogicDef value))
            {
                return value;
            }

            if (_source is not null && _source.LogicLookup.TryGetValue(name, out LogicClause c))
            {
                return InitLogicDef(name, c);
            }

            return null;
        }

        /// <summary>
        /// Fetches the logic def by name.
        /// </summary>
        /// <exception cref="ArgumentException">The logic def is not defined.</exception>
        public LogicDef GetLogicDefStrict(string name)
        {
            return GetLogicDef(name) ?? throw new ArgumentException($"LogicDef {name} is not defined.");
        }

        internal bool IsLogicDef(string reference)
        {
            return _logicDefs.ContainsKey(reference)
                || (_source is not null && _source.LogicLookup.ContainsKey(reference));
        }

        private DNFLogicDef InitLogicDef(string name, LogicClause lc)
        {
            _cycleDetector.PushReference(name);
            DNFLogicDef result = CreateDNFLogicDef(name, lc);
            _logicDefs.Add(name, result);
            _cycleDetector.Pop();
            return result;
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

        internal ILogicVariable? GetTermOrVariable(string name) => (ILogicVariable)GetTerm(name) ?? GetVariable(name);
        internal ILogicVariable GetTermOrVariableStrict(string name) => GetTermOrVariable(name) ?? throw new ArgumentException($"Unable to resolve {name} to Term or LogicVariable.");

        /// <summary>
        /// Fetches the logic item by name. Returns null if not defined.
        /// </summary>
        public LogicItem? GetItem(string name)
        {
            if (_items.TryGetValue(name, out LogicItem item))
            {
                return item;
            }

            if (_source is not null && _source.ItemLookup.TryGetValue(name, out ILogicItemTemplate t))
            {
                return InitItem(name, t);
            }

            return null;
        }

        /// <summary>
        /// Fetches the logic item by name.
        /// </summary>
        /// <exception cref="ArgumentException">The logic item is not defined.</exception>
        public LogicItem GetItemStrict(string name)
        {
            return GetItem(name) ?? throw new ArgumentException($"LogicItem {name} is not defined.");
        }

        private LogicItem InitItem(string name, ILogicItemTemplate t)
        {
            _cycleDetector.PushItem(name);
            LogicItem item = t.Create(this);
            _items.Add(name, item);
            _cycleDetector.Pop();
            return item;
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
                return new SingleStateRPNLogic(name, c, this);
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error in processing logic for {name}.", e);
            }
        }

        public RPNLogicDef CreateRPNLogicDef(RawLogicDef def) => CreateRPNLogicDef(def.name, new(def.logic));

        public RPNLogicDef CreateRPNLogicDef(string name, LogicClause c)
        {
            try
            {
                return new RPNLogicDef(name, c, this);
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error in processing RPN logic for {name}.", e);
            }
        }

        [Obsolete]
        public RPNLogicDef FromTokens(string name, LogicClause c) => CreateRPNLogicDef(name, c);

        public DNFLogicDef CreateDNFLogicDef(RawLogicDef def)
        {
            try
            {
                Expression<LogicExpressionType> expr = LogicExpressionUtil.Parse(def.logic);
                return new(def.name, def.logic, expr, this);
            }
            catch (Exception e)
            {
                throw new ArgumentException($"Error in processing logic for {def.name}.", e);
            }
        }
        public DNFLogicDef CreateDNFLogicDef(string name, LogicClause tokens) => new(name, tokens.ToInfix(), tokens.Expr, this);

        public LogicItem FromItemString(string name, string itemDef)
        {
            if (string.IsNullOrWhiteSpace(itemDef))
            {
                return new StringItem(name, itemDef, EmptyEffect.Instance);
            }
            StringItemBuilder builder = new(name, this);
            return new StringItem(name, itemDef, builder.ParseStringToEffect(itemDef));
        }

        public MacroDef? GetMacro(string name)
        {
            if (_macros.TryGetValue(name, out MacroDef? result))
            {
                return result;
            }

            if (_source is not null && _source.MacroLookup.TryGetValue(name, out LogicClause lc))
            {
                return InitMacro(name, lc);
            }

            return null;
        }

        public MacroDef GetMacroStrict(string name) => GetMacro(name) ?? throw new ArgumentException($"Macro {name} is not defined.");

        private MacroDef InitMacro(string name, LogicClause lc)
        {
            _cycleDetector.PushMacro(name);
            string infix = lc.ToInfix();
            var newExpr = lc.Expr.TransformRecursive((expr, eb) =>
            {
                switch (expr)
                {
                    case CoalesceExpression q:
                        return q.Left.IsDefined(this) ? q.Left : q.Right;
                    case LogicAtomExpression { Token.Content: string name }:
                        if (GetMacro(name) is MacroDef macro) return macro.Logic.Expr;
                        break;
                }
                return null;
            }, LogicExpressionUtil.Builder);
            _cycleDetector.Pop();
            MacroDef def = new(name, infix, new(newExpr));
            _macros.Add(name, def);
            return def;
        }
    }
}

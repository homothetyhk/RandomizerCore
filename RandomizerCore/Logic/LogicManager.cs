using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RandomizerCore.Json;
using static RandomizerCore.LogHelper;

namespace RandomizerCore.Logic
{
    public partial class LogicManager : ILogicManager
    {
        public readonly int TermCount;

        // Data structures dynamically constructed to correspond to logic
        readonly Dictionary<string, LogicDef> _logicDefs;
        readonly Dictionary<string, Term> _termLookup;
        readonly Term[] _terms;
        readonly ILogicProcessor _LP;
        readonly List<LogicInt> _variables;
        readonly Dictionary<string, int> _variableIndices;
        readonly Dictionary<string, LogicItem> _items;
        readonly Dictionary<string, LogicTransition> _transitions;
        readonly VariableResolver _variableResolver;
        public readonly LogicWaypoint[] Waypoints;

        public const int intVariableOffset = -100;

        public LogicManager(
            LogicProcessor lp,
            IEnumerable<string> terms,
            IEnumerable<RawLogicDef> logic,
            JArray unprocessedItems,
            IEnumerable<RawLogicDef> waypoints,
            IEnumerable<RawLogicTransition> transitions,
            VariableResolver variableResolver)
        {
            _LP = lp ?? new LogicProcessor(new());
            terms ??= Enumerable.Empty<string>();
            logic ??= Enumerable.Empty<RawLogicDef>();
            waypoints ??= Enumerable.Empty<RawLogicDef>();
            transitions ??= Enumerable.Empty<RawLogicTransition>();
            _variableResolver = variableResolver ?? new VariableResolver();
            _variables = new();
            _variableIndices = new();

            _termLookup = new();

            foreach (string t in terms) if (!_termLookup.ContainsKey(t)) _termLookup.Add(t, new(_termLookup.Count, t));
            foreach (var def in waypoints) if (!_termLookup.ContainsKey(def.name)) _termLookup.Add(def.name, new(_termLookup.Count, def.name));
            foreach (var def in transitions) if (!_termLookup.ContainsKey(def.Name)) _termLookup.Add(def.Name, new(_termLookup.Count, def.Name));
            if (unprocessedItems != null)
            {
                JsonSerializer js = JsonUtil.GetLogicSerializer(this);
                _items = unprocessedItems.ToObject<IEnumerable<LogicItem>>(js).ToDictionary(i => i.Name);
            }
            else _items = new();

            TermCount = _termLookup.Count;
            _terms = new Term[TermCount];
            foreach (var kvp in _termLookup) _terms[kvp.Value.Id] = kvp.Value;

            _logicDefs = new();
            foreach (RawLogicDef def in logic.Concat(waypoints).Concat(transitions.Select(t => new RawLogicDef(t.Name, t.logic))))
            {
                _logicDefs.Add(def.name, Process(def));
            }

            Waypoints = waypoints.Select(w => new LogicWaypoint(_termLookup[w.name], _logicDefs[w.name])).ToArray();

            _transitions = new();
            foreach (RawLogicTransition t in transitions)
            {
                _transitions.Add(t.Name, new LogicTransition(t, _termLookup[t.Name], _logicDefs[t.Name]));
            }
        }

        public LogicDef GetLogicDef(string name)
        {
            if (!_logicDefs.TryGetValue(name, out LogicDef def))
            {
                Log($"Unable to find logic for {name}.");
                return null;
            }

            return def;
        }

        public Term GetTerm(string item)
        {
            if (!_termLookup.TryGetValue(item, out Term index))
            {
                Log($"Unable to find index of term {item}.");
                return null;
            }

            return index;
        }

        public Term GetTerm(int id)
        {
            return _terms[id];
        }

        public int EvaluateVariable(object sender, ProgressionManager pm, int id)
        {
            return _variables[intVariableOffset - id].GetValue(sender, pm);
        }

        public LogicInt GetVariable(int id)
        {
            return _variables[intVariableOffset - id];
        }

        public LogicItem GetItem(string name)
        {
            if (!_items.TryGetValue(name, out LogicItem item))
            {
                Log($"Unable to find logic item for {name}.");
                return null;
            }

            return item;
        }

        public LogicTransition GetTransition(string name)
        {
            if (!_transitions.TryGetValue(name, out LogicTransition transition))
            {
                Log($"Unable to find logic transition for {name}.");
                return null;
            }

            return transition;
        }

        public LogicDef FromString(RawLogicDef def)
        {
            return Process(def);
        }

        private LogicDef Process(RawLogicDef def)
        {
            try
            {
                return new LogicDef(def.name, def.logic, Process(_LP.Shunt(def.logic)), this);
            }
            catch (Exception e)
            {
                Log($"Error in processing logic for {def.name}:\n{e.Message}");
                throw;
            }
        }

        private int[] Process(IList<string> rpn)
        {
            int[] maskedLogic = new int[rpn.Count];
            for (int i = 0; i < maskedLogic.Length; i++)
            {
                string s = rpn[i];
                if (LogicOperatorRef.TryGetOperator(s, out int op))
                {
                    maskedLogic[i] = op;
                }
                else if (_termLookup.TryGetValue(s, out Term t))
                {
                    maskedLogic[i] = t.Id;
                }
                else if (_variableIndices.TryGetValue(s, out int v))
                {
                    maskedLogic[i] = v;
                }
                else if (_variableResolver.TryMatch(this, s, out LogicInt variable))
                {
                    int index = intVariableOffset - _variables.Count;
                    maskedLogic[i] = _variableIndices[s] = index;
                    _variables.Add(variable);
                }
                else
                {
                    throw new ArgumentException($"Unknown token {s} found in logic");
                }
            }

            return maskedLogic;
        }
    }
}

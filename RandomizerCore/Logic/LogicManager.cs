using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using static RandomizerCore.LogHelper;

namespace RandomizerCore.Logic
{
    public interface ILogicManager
    {
        int GetTermIndex(string term);
        string GetTerm(int index);
        LogicDef FromString(RawLogicDef def);
    }

    public class LogicManager : ILogicManager
    {
        public readonly int TermCount;

        // Data structures dynamically constructed to correspond to logic
        readonly Dictionary<string, LogicDef> _logicDefs;
        readonly Dictionary<string, int> _termIndices;
        readonly string[] _terms;
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
            IEnumerable<LogicItemTemplate> items,
            IEnumerable<RawLogicDef> waypoints,
            IEnumerable<RawLogicTransition> transitions,
            VariableResolver variableResolver)
        {
            _LP = lp ?? new LogicProcessor(new());
            terms ??= Enumerable.Empty<string>();
            logic ??= Enumerable.Empty<RawLogicDef>();
            items ??= Enumerable.Empty<LogicItemTemplate>();
            waypoints ??= Enumerable.Empty<RawLogicDef>();
            transitions ??= Enumerable.Empty<RawLogicTransition>();
            _variableResolver = variableResolver ?? new VariableResolver();
            _variables = new();
            _variableIndices = new();

            _termIndices = new();
            foreach (string term in terms.Concat(items.SelectMany(i => i.GetItemFlags())).Concat(waypoints.Select(w => w.name)).Concat(transitions.Select(t => t.Name)))
            {
                if (!_termIndices.ContainsKey(term)) _termIndices.Add(term, _termIndices.Count);
            }

            TermCount = _termIndices.Count;
            _terms = new string[TermCount];
            foreach (var kvp in _termIndices) _terms[kvp.Value] = kvp.Key;

            _logicDefs = new();
            foreach (RawLogicDef def in logic.Concat(waypoints).Concat(transitions.Select(t => new RawLogicDef(t.Name, t.logic))))
            {
                _logicDefs.Add(def.name, Process(def));
            }

            _items = new();
            foreach (LogicItemTemplate template in items)
            {
                _items.Add(template.name, template.ToLogicItem(this));
            }

            Waypoints = waypoints.Select(w => new LogicWaypoint(_termIndices[w.name], _logicDefs[w.name])).ToArray();

            _transitions = new();
            foreach (RawLogicTransition t in transitions)
            {
                _transitions.Add(t.Name, new LogicTransition(t, _termIndices[t.Name], _logicDefs[t.Name]));
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

        public int GetTermIndex(string item)
        {
            if (!_termIndices.TryGetValue(item, out int index))
            {
                Log($"Unable to find index of term {item}.");
                return -1;
            }

            return index;
        }

        public string GetTerm(int id)
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
                return new LogicDef(def.name, Process(_LP.Shunt(def.logic)), this);
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
                int val;
                if (LogicOperatorRef.TryGetOperator(s, out val))
                {
                    maskedLogic[i] = val;
                }
                else if (_termIndices.TryGetValue(s, out val))
                {
                    maskedLogic[i] = val;
                }
                else if (_variableIndices.TryGetValue(s, out val))
                {
                    maskedLogic[i] = val;
                }
                else if (_variableResolver.TryMatch(this, s, out LogicInt variable))
                {
                    int index = intVariableOffset - _variables.Count;
                    maskedLogic[i] = _variableIndices[s] = index;
                    _variables.Add(variable);
                }
                /*
                else if (int.TryParse(s, out val))
                {
                    maskedLogic[i] = val;
                }
                */
                else
                {
                    throw new ArgumentException($"Unknown token {s} found in logic");
                }
            }

            return maskedLogic;
        }
    }
}

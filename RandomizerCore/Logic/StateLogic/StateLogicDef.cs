﻿using System.Diagnostics.CodeAnalysis;

namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A LogicDef which also supports state calculation.
    /// </summary>
    public abstract class StateLogicDef : LogicDef
    {
        public StateLogicDef(string name, string infixSource) : base(name, infixSource) { }

        /// <summary>
        /// Uses the current progression data to determine under what state(s) the logic is satisfiable.
        /// <br/>Appends a (possibly empty, non-reduced) sequence of result states to the input list. Returns true if the result is nonempty, or the result is empty but represents a nonnull indeterminate result.
        /// </summary>
        public abstract bool EvaluateState(ProgressionManager pm, List<State> result);
        /// <summary>
        /// Uses the current progression data what state(s) the logic is satisfiable, only allowing input states from the given state provider (recognized by reference equality).
        /// </summary>
        public abstract bool EvaluateStateFrom(ProgressionManager pm, IStateProvider stateProvider, List<State> result);
        /// <summary>
        /// Gets a sequence of unique state providers the LogicDef may depend on, for use with <see cref="EvaluateStateFrom(ProgressionManager, IStateProvider, List{State})"/>.
        /// </summary>
        public abstract IEnumerable<IStateProvider> GetStateProviders();
        /// <summary>
        /// Runs EvaluateState, and returns true if any new states are added to the state union, or if current is null and the result is empty.
        /// </summary>
        public virtual bool CheckForUpdatedState(ProgressionManager pm, StateUnion? current, List<State> accumulator, [MaybeNullWhen(false)] out StateUnion result)
        {
            bool succeedsOnEmpty = EvaluateState(pm, accumulator);
            if (current is null)
            {
                if (accumulator.Count > 0)
                {
                    result = new(accumulator);
                    return true;
                }
                else if (succeedsOnEmpty)
                {
                    result = pm.lm.StateManager.Empty;
                    return true;
                }
                else
                {
                    result = null;
                    return false;
                }
            }
            else return StateUnion.TryUnion(current, accumulator, out result);
        }
        /// <summary>
        /// Runs EvaluateState, and returns true if any new states are added to the state union.
        /// </summary>
        public virtual bool CheckForUpdatedState(ProgressionManager pm, StateUnion? current, List<State> accumulator, int modifiedTerm, [MaybeNullWhen(false)] out StateUnion result)
        {
            return CheckForUpdatedState(pm, current, accumulator, out result);
        }
    }
}

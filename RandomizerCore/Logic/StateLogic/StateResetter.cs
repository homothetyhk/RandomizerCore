namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// Modifier which outputs a specified state, with options to conditionally leave some fields unmodified.
    /// </summary>
    public abstract class StateResetter : StateModifier
    {
        /// <summary>
        /// The state which the resetter resets toward.
        /// </summary>
        protected abstract State ResetState { get; }
        /// <summary>
        /// If not null, the name of a state field property which stores infix logic, which evaluates to true when the state field should be modified by the resetter.
        /// </summary>
        protected abstract string? ResetLogicProperty { get; }
        /// <summary>
        /// By default, is false, and fields which do not declare reset logic are reset. If true, fields which do not declare reset logic are not reset.
        /// </summary>
        protected virtual bool OptIn { get; }

        private readonly (StateField sf, SingleStateLogic logic)[] properties;

        protected StateResetter(LogicManager lm)
        {
            string? rlp = ResetLogicProperty;

            List<(StateField sf, SingleStateLogic logic)> ps = new();
            foreach (var kvp in lm.StateManager.FieldProperties)
            {
                if (rlp is not null && kvp.Value.TryGetValue(rlp, out object? ro) && ro is string ri)
                {
                    SingleStateLogic ssl = lm.CreateSingleStateLogic(new($"{rlp}-{kvp.Key}", ri));
                    ps.Add((lm.StateManager.FieldLookup[kvp.Key], ssl));
                }
            }
            properties = ps.ToArray();
        }

        /// <summary>
        /// For a StateResetter, returns the empty sequence, unless overridden by a derived class.
        /// </summary>
        public override IEnumerable<LazyStateBuilder>? ProvideState(object? sender, ProgressionManager pm)
        {
            return Enumerable.Empty<LazyStateBuilder>();
        }

        /// <summary>
        /// For a StateResetter, returns a singleton containing the result of ResetSingle, unless overridden by a derived class.
        /// </summary>
        public override IEnumerable<LazyStateBuilder> ModifyState(object? sender, ProgressionManager pm, LazyStateBuilder state)
        {
            yield return ResetSingle(pm, state);
        }

        /// <summary>
        /// Applies the result of resetting the state towards the ResetState, accounting for the OptIn and conditional reset properties for this resetter.
        /// </summary>
        protected LazyStateBuilder ResetSingle(ProgressionManager pm, LazyStateBuilder state)
        {
            return OptIn ? ResetOptIn(pm, state) : ResetOptOut(pm, state);
        }

        private LazyStateBuilder ResetOptOut(ProgressionManager pm, LazyStateBuilder orig)
        {
            LazyStateBuilder lsb = new(ResetState);

            foreach (var (sf, logic) in properties)
            {
                if (!logic.CanGet(pm, orig))
                {
                    if (sf is StateBool)
                    {
                        lsb.SetBool(sf, orig.GetBool(sf));
                    }
                    else
                    {
                        lsb.SetInt(sf, orig.GetInt(sf));
                    }
                }
            }

            return lsb;
        }

        private LazyStateBuilder ResetOptIn(ProgressionManager pm, LazyStateBuilder orig)
        {
            State s = orig.GetState();
            LazyStateBuilder lsb = new(s);

            foreach (var (sf, logic) in properties)
            {
                if (logic.CanGet(pm, s))
                {
                    if (sf is StateBool)
                    {
                        lsb.SetBool(sf, ResetState.GetBool(sf));
                    }
                    else
                    {
                        lsb.SetInt(sf, ResetState.GetInt(sf));
                    }
                }
            }
            return lsb;
        }

        public override IEnumerable<Term> GetTerms()
        {
            return properties.SelectMany(p => p.logic.GetTerms());
        }
    }
}

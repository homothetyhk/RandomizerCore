namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A <see cref="StateProviderVariable"/> which provides the start state.
    /// </summary>
    public sealed class StartStateProvider : StateProviderVariable
    {
        public override string Name => Prefix;
        public const string Prefix = "$START";

        public static bool TryMatch(LogicManager lm, string term, out LogicVariable variable)
        {
            if (term == Prefix)
            {
                variable = new StartStateProvider();
                return true;
            }
            variable = default;
            return false;
        }

        public override StateUnion? GetInputState(object sender, ProgressionManager pm)
        {
            return pm.lm.StateManager.StartStateSingleton;
        }

        public override IEnumerable<Term> GetTerms()
        {
            return Enumerable.Empty<Term>();
        }
    }
}

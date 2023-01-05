namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A <see cref="StateProvider"/> which provides the start state.
    /// </summary>
    public sealed class DefaultStateProvider : StateProvider
    {
        public override string Name => Prefix;
        public const string Prefix = "$DEFAULTSTATE";

        public static bool TryMatch(LogicManager lm, string term, out LogicVariable variable)
        {
            if (term == Prefix)
            {
                variable = new DefaultStateProvider();
                return true;
            }
            variable = default;
            return false;
        }

        public override StateUnion? GetInputState(object? sender, ProgressionManager pm)
        {
            return pm.lm.StateManager.DefaultStateSingleton;
        }

        public override IEnumerable<Term> GetTerms()
        {
            return Enumerable.Empty<Term>();
        }
    }
}

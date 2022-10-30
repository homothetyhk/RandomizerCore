namespace RandomizerCore.Logic.StateLogic
{
    /// <summary>
    /// A <see cref="StateProviderVariable"/> which provides the zero state.
    /// </summary>
    public sealed class ZeroStateProvider : StateProviderVariable
    {
        public override string Name => Prefix;
        public const string Prefix = "$START";

        public override StateUnion? GetInputState(object sender, ProgressionManager pm)
        {
            return pm.lm.StateManager.AbsorbingSet;
        }

        public override IEnumerable<Term> GetTerms()
        {
            return Enumerable.Empty<Term>();
        }
    }
}

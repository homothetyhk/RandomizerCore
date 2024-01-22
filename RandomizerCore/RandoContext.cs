using RandomizerCore.Logic;

namespace RandomizerCore
{
    public abstract class RandoContext
    {
        public readonly LogicManager LM;
        public ILogicItem InitialProgression;

        public RandoContext(LogicManager LM)
        {
            this.LM = LM;
        }

        public abstract IEnumerable<GeneralizedPlacement> EnumerateExistingPlacements();
    }
}

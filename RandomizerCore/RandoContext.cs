using Newtonsoft.Json;
using RandomizerCore.Json;
using RandomizerCore.Logic;

namespace RandomizerCore
{
    [JsonConverter(typeof(RandoContextConverter))]
    public abstract class RandoContext
    {
        public LogicManager LM;
        public ILogicItem InitialProgression;

        public RandoContext(LogicManager LM)
        {
            this.LM = LM;
        }

        public abstract IEnumerable<GeneralizedPlacement> EnumerateExistingPlacements();
    }
}

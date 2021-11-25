using Newtonsoft.Json;

namespace RandomizerCore.Logic
{
    
    public readonly record struct LogicTransitionData(string SceneName, string GateName, OneWayType OneWayType)
    {
        public string Name => $"{SceneName}[{GateName}]";
    }

    public readonly struct RawLogicTransition
    {
        [JsonConstructor]
        public RawLogicTransition(string sceneName, string gateName, string logic, OneWayType oneWayType)
        {
            this.sceneName = sceneName;
            this.gateName = gateName;
            this.logic = logic;
            this.oneWayType = oneWayType;
        }

        public LogicTransitionData GetTransitionData() => new(sceneName, gateName, oneWayType);

        public string Name => $"{sceneName}[{gateName}]";
        public readonly string sceneName;
        public readonly string gateName;
        public readonly string logic;
        public readonly OneWayType oneWayType;
    }
}

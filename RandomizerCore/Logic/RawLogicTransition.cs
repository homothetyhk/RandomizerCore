namespace RandomizerCore.Logic
{
    public readonly struct RawLogicTransition
    {
        [Newtonsoft.Json.JsonConstructor]
        public RawLogicTransition(string sceneName, string gateName, string logic, OneWayType oneWayType)
        {
            this.sceneName = sceneName;
            this.gateName = gateName;
            this.logic = logic;
            this.oneWayType = oneWayType;
        }

        public string Name => $"{sceneName}[{gateName}]";
        public readonly string sceneName;
        public readonly string gateName;
        public readonly string logic;
        public readonly OneWayType oneWayType;
    }
}

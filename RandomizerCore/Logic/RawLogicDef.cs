namespace RandomizerCore.Logic
{
    public readonly struct RawLogicDef
    {
        [Newtonsoft.Json.JsonConstructor]
        public RawLogicDef(string name, string logic)
        {
            this.name = name;
            this.logic = logic;
        }

        public readonly string name;
        public readonly string logic;
    }
}

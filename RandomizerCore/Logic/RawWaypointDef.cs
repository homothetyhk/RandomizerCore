namespace RandomizerCore.Logic
{
    public readonly struct RawWaypointDef
    {
        public RawWaypointDef(string name, string logic, bool stateless = false)
        {
            this.name = name;
            this.logic = logic;
            this.stateless = stateless;
        }

        public readonly string name;
        public readonly string logic;
        public readonly bool stateless;

        public static implicit operator RawLogicDef(RawWaypointDef def) => new(def.name, def.logic);
    }

}

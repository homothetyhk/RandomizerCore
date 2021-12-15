namespace RandomizerCore
{
    public interface IRandoItem : ILogicItem
    {
        public int Priority { get; set; }
        public State Placed { get; set; }
    }
}

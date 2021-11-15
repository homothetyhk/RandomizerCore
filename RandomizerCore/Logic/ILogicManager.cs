namespace RandomizerCore.Logic
{
    public interface ILogicManager
    {
        Term GetTerm(string term);
        Term GetTerm(int index);
        OptimizedLogicDef FromString(RawLogicDef def);
    }
}

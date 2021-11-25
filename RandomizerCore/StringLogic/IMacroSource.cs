namespace RandomizerCore.StringLogic
{
    public interface IMacroSource
    {
        LogicClause GetMacro(string name);
    }
}

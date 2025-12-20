namespace RandomizerCore.StringLogic
{
    [Obsolete]
    public interface IMacroSource
    {
        LogicClause GetMacro(string name);
    }
}

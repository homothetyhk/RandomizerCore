namespace RandomizerCore.StringLogic
{
    [Obsolete]
    public interface ITokenSource
    {
        TermToken GetTermToken(string name);
        ComparisonToken GetComparisonToken(ComparisonType comparisonType, string left, string right);
    }
}

namespace RandomizerCore.Logic
{
    public class VariableResolver
    {
        public virtual bool TryMatch(LogicManager lm, string term, out LogicInt variable)
        {
            if (int.TryParse(term, out int value))
            {
                variable = new ConstantInt(value);
                return true;
            }

            variable = null;
            return false;
        }
    }
}

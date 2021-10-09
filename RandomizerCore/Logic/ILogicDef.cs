using System.Collections.Generic;

namespace RandomizerCore.Logic
{
    public interface ILogicDef
    {
        string Name { get; }
        bool CanGet(ProgressionManager pm);
        IEnumerable<Term> GetTerms();
    }
}

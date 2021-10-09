using System.Collections.Generic;

namespace RandomizerCore.Logic
{
    public interface ILogicProcessor
    {
        IList<string> Shunt(string infix);
    }
}

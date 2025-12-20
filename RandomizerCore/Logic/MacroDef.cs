using RandomizerCore.StringLogic;

namespace RandomizerCore.Logic
{
    public class MacroDef
    {
        public string Name { get; }
        public string InfixSource { get; }
        /// <summary>
        /// The parsed logic of the macro, simplified by: all macro usages substituted, all coalesce expressions evaluated.
        /// </summary>
        public LogicClause Logic { get; }

        internal MacroDef(string name, string infix, LogicClause logic)
        {
            Name = name;
            InfixSource = infix;
            Logic = logic;
        }
    }
}

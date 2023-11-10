using RandomizerCore.Logic;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.LogicItems.Templates
{
    public record FirstOfItemTemplate(string Name, IEnumerable<ILogicItemTemplate> NestedItems) : LogicItemTemplate<FirstOfItem>(Name)
    {
        public override FirstOfItem Create(LogicManager lm)
        {
            return new(Name, NestedItems.Select(i => i.Create(lm)));
        }
    }
}

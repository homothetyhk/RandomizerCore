using RandomizerCore.Logic;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.LogicItems.Templates
{
    public record AllOfItemTemplate(string Name, IEnumerable<ILogicItemTemplate> NestedItems) : LogicItemTemplate<AllOfItem>(Name)
    {
        public override AllOfItem Create(LogicManager lm)
        {
            return new(Name, NestedItems.Select(i => i.Create(lm)));
        }
    }
}

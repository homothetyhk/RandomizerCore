using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems.Templates
{
    public record BoolItemTemplate(string Name, string Term) : LogicItemTemplate<BoolItem>(Name)
    {
        public override BoolItem Create(LogicManager lm)
        {
            return new(Name, lm.GetTermStrict(Term));
        }
    }

}

using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems.Templates
{
    public record EmptyItemTemplate(string Name) : LogicItemTemplate<EmptyItem>(Name)
    {
        public override EmptyItem Create(LogicManager lm)
        {
            return new(Name);
        }
    }
}

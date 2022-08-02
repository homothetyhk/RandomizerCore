using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems.Templates
{
    public record BranchedItemTemplate(string Name, string Logic, ILogicItemTemplate TrueItem, ILogicItemTemplate FalseItem) : LogicItemTemplate<BranchedItem>(Name)
    {
        public override BranchedItem Create(LogicManager lm)
        {
            return new(Name, lm.FromString(new(Name, Logic)), TrueItem.Create(lm), FalseItem.Create(lm));
        }
    }
}

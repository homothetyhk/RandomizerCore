using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems.Templates
{
    public record SingleItemTemplate(string Name, (string Term, int Value) Effect) : LogicItemTemplate<SingleItem>(Name)
    {
        public override SingleItem Create(LogicManager lm)
        {
            return new(Name, new(lm.GetTerm(Effect.Term), Effect.Value));
        }
    }
}

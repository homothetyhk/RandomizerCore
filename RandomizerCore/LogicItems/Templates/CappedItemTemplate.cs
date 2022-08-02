using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems.Templates
{
    public record CappedItemTemplate(string Name, (string Term, int Value)[] Effects, (string Term, int Value) Cap) : LogicItemTemplate<CappedItem>(Name)
    {
        public override CappedItem Create(LogicManager lm)
        {
            return new(Name, Effects.Select(p => new TermValue(lm.GetTerm(p.Term), p.Value)).ToArray(), new(lm.GetTerm(Cap.Term), Cap.Value));
        }
    }
}

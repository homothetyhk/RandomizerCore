using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems.Templates
{
    public record MultiItemTemplate(string Name, (string Term, int Value)[] Effects) : LogicItemTemplate<MultiItem>(Name)
    {
        public override MultiItem Create(LogicManager lm)
        {
            return new(Name, Effects.Select(p => new TermValue(lm.GetTermStrict(p.Term), p.Value)).ToArray());
        }
    }
}

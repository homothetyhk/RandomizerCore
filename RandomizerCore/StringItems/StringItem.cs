using RandomizerCore.Logic;
using RandomizerCore.LogicItems;

namespace RandomizerCore.StringItems
{
    public sealed record StringItem(string Name, string InfixSource, StringItemEffect Effect) : LogicItem(Name), IConditionalItem
    {
        public override void AddTo(ProgressionManager pm)
        {
            Effect.AddTo(pm);
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            return Effect.GetAffectedTerms();
        }

        public bool CheckForEffect(ProgressionManager pm)
        {
            return Effect.CheckForEffect(pm);
        }

        public bool TryAddTo(ProgressionManager pm)
        {
            return Effect.AddTo(pm);
        }
    }

    public sealed record StringItemTemplate(string Name, string InfixSource) : LogicItemTemplate<StringItem>(Name)
    {
        public override StringItem Create(LogicManager lm)
        {
            return (StringItem)lm.FromItemString(Name, InfixSource);
        }
    }

    public abstract record StringItemEffect
    {
        public abstract bool AddTo(ProgressionManager pm);
        public abstract bool CheckForEffect(ProgressionManager pm);
        public abstract IEnumerable<Term> GetAffectedTerms();
    }

    public sealed record EmptyEffect : StringItemEffect
    {
        public static EmptyEffect Instance { get; } = new EmptyEffect();
        public override bool AddTo(ProgressionManager pm) => false;
        public override bool CheckForEffect(ProgressionManager pm) => false;
        public override IEnumerable<Term> GetAffectedTerms() => Enumerable.Empty<Term>();
    }

    public record AllOfEffect : StringItemEffect
    {
        private readonly StringItemEffect[] Effects;

        public AllOfEffect(StringItemEffect[] effects)
        {
            Effects = effects;
        }

        public override bool AddTo(ProgressionManager pm)
        {
            bool result = false;
            foreach (StringItemEffect effect in Effects) result |= effect.AddTo(pm);
            return result;
        }

        public override bool CheckForEffect(ProgressionManager pm)
        {
            return Effects.Any(e => e.CheckForEffect(pm));
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            return Effects.SelectMany(e => e.GetAffectedTerms());
        }
    }

    public record FirstOfEffect : StringItemEffect
    {
        private readonly StringItemEffect[] Effects;

        public FirstOfEffect(StringItemEffect[] effects)
        {
            Effects = effects;
        }

        public override bool AddTo(ProgressionManager pm)
        {
            foreach (StringItemEffect effect in Effects) if (effect.AddTo(pm)) return true;
            return false;
        }

        public override bool CheckForEffect(ProgressionManager pm)
        {
            return Effects.Any(e => e.CheckForEffect(pm));
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            return Effects.SelectMany(e => e.GetAffectedTerms());
        }
    }

    public record IncrementEffect(int Value, Term Term) : StringItemEffect
    {
        public override bool AddTo(ProgressionManager pm)
        {
            pm.Incr(Term, Value);
            return true;
        }

        public override bool CheckForEffect(ProgressionManager pm)
        {
            return true;
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            yield return Term;
        }
    }

    public record MaxWithEffect(int Value, Term Term) : StringItemEffect
    {
        public override bool AddTo(ProgressionManager pm)
        {
            if (Value > pm.Get(Term))
            {
                pm.Set(Term, Value);
                return true;
            }
            return false;
        }

        public override bool CheckForEffect(ProgressionManager pm)
        {
            return pm.Get(Term) < Value;
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            yield return Term;
        }
    }

    public record ConditionalEffect(LogicDef Logic, StringItemEffect Effect, bool Negated = false) : StringItemEffect
    {
        public override bool AddTo(ProgressionManager pm)
        {
            if (Logic.CanGet(pm) ^ Negated)
            {
                return Effect.AddTo(pm);
            }
            return false;
        }

        public override bool CheckForEffect(ProgressionManager pm)
        {
            return Logic.CanGet(pm) && Effect.CheckForEffect(pm);
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            return Effect.GetAffectedTerms();
        }
    }

    public record ReferenceEffect(LogicItem Item) : StringItemEffect
    {
        public override bool AddTo(ProgressionManager pm)
        {
            if (Item is StringItem { Effect: StringItemEffect e })
            {
                return e.AddTo(pm);
            }
            else
            {
                bool ret = Item is not IConditionalItem icl || icl.CheckForEffect(pm);
                if (ret) Item.AddTo(pm);
                return ret;
            }
        }

        public override bool CheckForEffect(ProgressionManager pm)
        {
            return Item is not IConditionalItem icl || icl.CheckForEffect(pm);
        }

        public override IEnumerable<Term> GetAffectedTerms()
        {
            return Item.GetAffectedTerms();
        }
    }
}

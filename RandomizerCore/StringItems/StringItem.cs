using RandomizerCore.Logic;
using RandomizerCore.LogicItems;
using RandomizerCore.StringParsing;

namespace RandomizerCore.StringItems
{
    public sealed record StringItem(string Name, string EffectString, StringItemEffect Effect) : LogicItem(Name), IConditionalItem
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

    public sealed record StringItemTemplate(string Name, string Effect) : LogicItemTemplate<StringItem>(Name)
    {
        public override StringItem Create(LogicManager lm)
        {
            return (StringItem)lm.FromItemString(Name, Effect);
        }
    }

    public abstract record StringItemEffect
    {
        public abstract bool AddTo(ProgressionManager pm);
        public abstract bool CheckForEffect(ProgressionManager pm);
        public abstract IEnumerable<Term> GetAffectedTerms();
        /// <summary>
        /// Converts the effect to an expression tree, with standardized formatting. May differ from the expression implied by <see cref="StringItem.EffectString"/>.
        /// </summary>
        public abstract IExpression<ItemExpressionType> ToExpression();
        /// <summary>
        /// Converts the effect to an effect string. By default, this prints the result of <see cref="ToExpression"/>, and may differ from <see cref="StringItem.EffectString"/>.
        /// </summary>
        public virtual string ToEffectString() => ToExpression().Print();
        internal static ItemExpressionFactory ExpressionFactory { get; } = new ItemExpressionFactory();
        internal static ItemOperatorProvider OperatorProvider { get; } = new ItemOperatorProvider();
        internal static ExpressionBuilder<ItemExpressionType> ExpressionBuilder { get; } = new(OperatorProvider, ExpressionFactory);
    }

    public sealed record EmptyEffect : StringItemEffect
    {
        public static EmptyEffect Instance { get; } = new EmptyEffect();
        public override bool AddTo(ProgressionManager pm) => false;
        public override bool CheckForEffect(ProgressionManager pm) => false;
        public override IEnumerable<Term> GetAffectedTerms() => Enumerable.Empty<Term>();
        public override IExpression<ItemExpressionType> ToExpression() => ExpressionBuilder.NameAtom(ItemExpressionFactory.EmptyEffect);
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

        public override IExpression<ItemExpressionType> ToExpression()
        {
            return ExpressionBuilder.ApplyInfixOperatorLeftAssoc(Effects.Select(e => e.ToExpression()), ExpressionBuilder.Op(ItemOperatorProvider.Chaining));
        }

        public virtual bool Equals(AllOfEffect? other)
        {
            return other is not null && other.Effects.SequenceEqual(Effects);
        }

        public override int GetHashCode()
        {
            return Effects[0].GetHashCode() ^ Effects.Length;
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


        public override IExpression<ItemExpressionType> ToExpression()
        {
            return ExpressionBuilder.ApplyInfixOperatorLeftAssoc(Effects.Select(e => e.ToExpression()), ExpressionBuilder.Op(ItemOperatorProvider.ShortCircuitChaining));
        }

        public virtual bool Equals(FirstOfEffect? other)
        {
            return other is not null && other.Effects.SequenceEqual(Effects);
        }

        public override int GetHashCode()
        {
            return Effects[0].GetHashCode() ^ Effects.Length;
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

        public override IExpression<ItemExpressionType> ToExpression()
        {
            if (Value == 1)
            {
                return ExpressionBuilder.ApplyPostfixOperator(ExpressionBuilder.NameAtom(Term.Name), ExpressionBuilder.Op(ItemOperatorProvider.Increment));
            }
            else
            {
                return ExpressionBuilder.ApplyInfixOperator(ExpressionBuilder.NameAtom(Term.Name), ExpressionBuilder.Op(ItemOperatorProvider.AdditionAssignment), ExpressionBuilder.NumberAtom(Value));
            }
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

        public override IExpression<ItemExpressionType> ToExpression()
        {
            return ExpressionBuilder.ApplyInfixOperator(ExpressionBuilder.NameAtom(Term.Name), ExpressionBuilder.Op(ItemOperatorProvider.MaxAssignment), ExpressionBuilder.NumberAtom(Value));
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

        public override IExpression<ItemExpressionType> ToExpression()
        {
            IExpression<ItemExpressionType> logicExpr = ExpressionBuilder.StringAtom(Logic.InfixSource);
            if (Negated) logicExpr = ExpressionBuilder.ApplyPrefixOperator(ExpressionBuilder.Op(ItemOperatorProvider.Negation), logicExpr);
            return ExpressionBuilder.ApplyInfixOperator(logicExpr, ExpressionBuilder.Op(ItemOperatorProvider.Conditional), Effect.ToExpression());
        }

        public virtual bool Equals(ConditionalEffect? other)
        {
            return other is not null && other.Negated == Negated && other.Logic.GetType() == Logic.GetType() && other.Logic.InfixSource == Logic.InfixSource;
        }

        public override int GetHashCode()
        {
            return Logic.InfixSource.GetHashCode() * (Negated ? 2 : 1);
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

        public override IExpression<ItemExpressionType> ToExpression()
        {
            return ExpressionBuilder.ApplyPrefixOperator(ExpressionBuilder.Op(ItemOperatorProvider.Reference), ExpressionBuilder.NameAtom(Item.Name));
        }
    }
}

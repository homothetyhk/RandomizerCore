using RandomizerCore.Logic;
using RandomizerCore.StringParsing;

namespace RandomizerCore.StringItems
{
    public class ItemExpessionBuilder : ExpressionBuilder<ItemExpressionType>
    {
        public ItemExpessionBuilder() : base(ItemExpressionUtil.OperatorProvider, ItemExpressionUtil.Factory) { }
    }

    public static class ItemExpressionUtil
    {
        public static ItemOperatorProvider OperatorProvider { get; } = new();
        public static ItemExpressionFactory Factory { get; } = new();
        public static ItemExpessionBuilder Builder { get; } = new();

        public static Expression<ItemExpressionType> Parse(string itemDef)
        {
            List<Token> tokens = Tokenizer.Tokenize(itemDef, OperatorProvider, '`');
            ExpressionParser<ItemExpressionType> p = new(OperatorProvider, Factory, tokens);
            return p.Parse();
        }
    }

    internal class StringItemBuilder
    {
        private readonly LogicManager lm;
        private int anonymousCount = 0; // used to generate names for named subterms.
        public string Name { get; }

        public StringItemBuilder(string name, LogicManager lm)
        {
            this.Name = name;
            this.lm = lm;
        }

        public StringItemEffect ParseStringToEffect(string itemDef)
        {
            Expression<ItemExpressionType> e = ItemExpressionUtil.Parse(itemDef);
            return ProcessItemExpressionToEffect(e);
        }


        private StringItemEffect ProcessItemExpressionToEffect(Expression<ItemExpressionType> expr)
        {
            switch (expr)
            {
                case GroupingExpression<ItemExpressionType> g:
                    return ProcessItemExpressionToEffect(g.Nested);
                case EmptyEffectExpression eee:
                    return EmptyEffect.Instance;
                case ReferenceExpression r:
                    {
                        string reference = ((NameToken)((ItemAtomExpression)r.Operand).Token).Content;
                        LogicItem item = lm.GetItemStrict(reference);
                        return new ReferenceEffect(item);
                    }
                case IncrementExpression i:
                    {
                        Term? t = ProcessItemExpressionToTerm(i.Operand);
                        return t is not null ? new IncrementEffect(1, t) : EmptyEffect.Instance;
                    }
                case AdditionAssignmentExpression aa:
                    {
                        Term? t = ProcessItemExpressionToTerm(aa.Left);
                        int argR = ((NumberToken)((ItemAtomExpression)aa.Right).Token).Value;
                        return t is not null ? new IncrementEffect(argR, t) : EmptyEffect.Instance;
                    }
                case MaxAssignmentExpression mx:
                    {
                        Term? t = ProcessItemExpressionToTerm(mx.Left);
                        int argR = ((NumberToken)((ItemAtomExpression)mx.Right).Token).Value;
                        return t is not null ? new IncrementEffect(argR, t) : EmptyEffect.Instance;
                    }
                case ConditionalExpression b:
                    {
                        ProcessItemExpressiontoBool(b.Left, out LogicDef logic, out bool negated);
                        StringItemEffect e = ProcessItemExpressionToEffect(b.Right);
                        return new ConditionalEffect(logic, e, negated);
                    }
                case ShortCircuitChainingExpression sc:
                    {
                        return new FirstOfEffect(ShortCircuitChainingExpression.FlattenAssoc(sc)
                            .Select(e => ProcessItemExpressionToEffect(e)).ToArray());
                    }
                case ChainingExpression c:
                    {
                        return new AllOfEffect(ChainingExpression.FlattenAssoc(c)
                            .Select(e => ProcessItemExpressionToEffect(e)).ToArray());
                    }
                default:
                    throw UnknownItemExpressionTypeError(expr, "item");
            }
        }

        private void ProcessItemExpressiontoBool(Expression<ItemExpressionType> expr, out LogicDef def, out bool negated)
        {
            if (expr is NegationExpression n)
            {
                ProcessItemExpressiontoBool(n.Operand, out def, out negated);
                negated ^= true;
            }
            else if (expr is ItemAtomExpression a)
            {
                string logic = ((StringToken)a.Token).Content;
                def = lm.FromString(new(GenerateAnonymousLogicName(logic), logic));
                negated = false;
            }
            else throw UnknownItemExpressionTypeError(expr, "bool");
        }

        private Term? ProcessItemExpressionToTerm(Expression<ItemExpressionType> expr)
        {
            return expr switch
            {
                ItemAtomExpression a => lm.GetTermStrict(((NameToken)a.Token).Content),
                CoalescingExpression q => lm.GetTerm(((NameToken)((ItemAtomExpression)q.Operand).Token).Content),
                _ => throw UnknownItemExpressionTypeError(expr, "term"),
            };
        }

        private string GenerateAnonymousLogicName(string infix)
        {
            return $"{Name}.Anonymous{anonymousCount++}{{{infix}}}";
        }

        private Exception UnknownItemExpressionTypeError(Expression<ItemExpressionType> expr, string expectedType = null)
        {
            string msg = $"Unknown expression {expr.Print()} found in item {Name}.";
            if (expectedType != null) msg += $" Expected type: {expectedType}.";
            return new ArgumentException(msg);
        }
    }
}

using RandomizerCore.Logic;
using RandomizerCore.StringParsing;

namespace RandomizerCore.StringItems
{
    internal class StringItemBuilder
    {
        private readonly LogicManager lm;
        private readonly ItemOperatorProvider _itemOperatorProvider = new();
        private readonly ItemExpressionFactory _itemExpressionFactory = new();

        public StringItemBuilder(LogicManager lm)
        {
            this.lm = lm;
        }

        public StringItemEffect ParseStringToEffect(string name, string itemDef)
        {
            Tokenizer t = new(_itemOperatorProvider, itemDef, '`');
            List<Token> tokens = t.Tokenize();
            ExpressionParser<ItemExpressionType> p = new(_itemOperatorProvider, _itemExpressionFactory, tokens);
            IExpression<ItemExpressionType> e = p.Parse();
            return ProcessItemExpressionToEffect(name, e);
        }


        private StringItemEffect ProcessItemExpressionToEffect(string name, IExpression<ItemExpressionType> expr)
        {
            switch (expr)
            {
                case GroupingExpression<ItemExpressionType> g:
                    return ProcessItemExpressionToEffect(name, g.Nested);
                case EmptyEffectExpression eee:
                    return EmptyEffect.Instance;
                case ReferenceExpression r:
                    {
                        string reference = ((NameToken)((ItemAtomExpression)r.Operand).Token).Value;
                        LogicItem item = lm.ResolveItemReference(name, reference);
                        return new ReferenceEffect(item);
                    }
                case IncrementExpression i:
                    {
                        Term? t = ProcessItemExpressionToTerm(name, i.Operand);
                        return t is not null ? new IncrementEffect(1, t) : EmptyEffect.Instance;
                    }
                case AdditionAssignmentExpression aa:
                    {
                        Term? t = ProcessItemExpressionToTerm(name, aa.Left);
                        int argR = ((NumberToken)((ItemAtomExpression)aa.Right).Token).Value;
                        return t is not null ? new IncrementEffect(argR, t) : EmptyEffect.Instance;
                    }
                case MaxAssignmentExpression mx:
                    {
                        Term? t = ProcessItemExpressionToTerm(name, mx.Left);
                        int argR = ((NumberToken)((ItemAtomExpression)mx.Right).Token).Value;
                        return t is not null ? new IncrementEffect(argR, t) : EmptyEffect.Instance;
                    }
                case ConditionalExpression b:
                    {
                        ProcessItemExpressiontoBool(name, b.Left, out LogicDef logic, out bool negated);
                        StringItemEffect e = ProcessItemExpressionToEffect(name, b.Right);
                        return new ConditionalEffect(logic, e, negated);
                    }
                case ShortCircuitChainingExpression sc:
                    {
                        return new FirstOfEffect(ShortCircuitChainingExpression.FlattenAssoc(sc)
                            .Select(e => ProcessItemExpressionToEffect(name, e)).ToArray());
                    }
                case ChainingExpression c:
                    {
                        return new AllOfEffect(ChainingExpression.FlattenAssoc(c)
                            .Select(e => ProcessItemExpressionToEffect(name, e)).ToArray());
                    }
                default:
                    throw UnknownItemExpressionTypeError(name, expr, "item");
            }
        }

        private void ProcessItemExpressiontoBool(string name, IExpression<ItemExpressionType> expr, out LogicDef def, out bool negated)
        {
            if (expr is NegationExpression n)
            {
                ProcessItemExpressiontoBool(name, n.Operand, out def, out negated);
                negated ^= true;
            }
            else if (expr is ItemAtomExpression a)
            {
                string logic = ((StringToken)a.Token).Value;
                def = lm.FromString(new(name, logic));
                negated = false;
            }
            else throw UnknownItemExpressionTypeError(name, expr, "bool");
        }

        private Term? ProcessItemExpressionToTerm(string name, IExpression<ItemExpressionType> expr)
        {
            return expr switch
            {
                ItemAtomExpression a => lm.GetTermStrict(((NameToken)a.Token).Value),
                CoalescingExpression q => lm.GetTerm(((NameToken)((ItemAtomExpression)q.Operand).Token).Value),
                _ => throw UnknownItemExpressionTypeError(name, expr, "term"),
            };
        }

        private static Exception UnknownItemExpressionTypeError(string name, IExpression<ItemExpressionType> expr, string expectedType = null)
        {
            string msg = $"Unknown expression {expr.Print()} found in item {name}.";
            if (expectedType != null) msg += $" Expected type: {expectedType}.";
            return new ArgumentException(msg);
        }
    }
}

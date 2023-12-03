using RandomizerCore.Logic;
using RandomizerCore.StringItems;
using RandomizerCore.StringParsing;

namespace RandomizerCore.LogicItems
{
    public static class StringItemConversion
    {
        private static readonly ExpressionBuilder<ItemExpressionType> eb = new(new ItemOperatorProvider(), new ItemExpressionFactory());


        public static bool TryConvertToItemString(LogicItem item, out string itemString)
        {
            bool result = TryConvertToItemExpression(item, out IExpression<ItemExpressionType> expr);
            itemString = result ? expr.Print() : null;
            return result;
        }

        public static IExpression<ItemExpressionType> Convert(SingleItem item) => item.Effect.Value == 1
                ? eb.ApplyPostfixOperator(eb.NameAtom(item.Effect.Term.Name), eb.Op(ItemOperatorProvider.Increment))
                : eb.ApplyInfixOperator(eb.NameAtom(item.Effect.Term.Name), eb.Op(ItemOperatorProvider.AdditionAssignment), eb.NumberAtom(item.Effect.Value));

        public static IExpression<ItemExpressionType> Convert(MultiItem item) => item.Effects.Select(e => Convert(new SingleItem(item.Name, e)))
                .Aggregate((iL, iR) => eb.ApplyInfixOperator(iL, eb.Op(ItemOperatorProvider.Chaining), iR));

        public static IExpression<ItemExpressionType> Convert(EmptyItem item) => eb.NameAtom(ItemExpressionFactory.EmptyEffect);

        public static IExpression<ItemExpressionType> Convert(BoolItem item) => eb.ApplyInfixOperator(eb.NameAtom(item.Term.Name), eb.Op(ItemOperatorProvider.MaxAssignment), eb.NumberAtom(1));

        public static IExpression<ItemExpressionType> Convert(CappedItem item) => eb.ApplyInfixOperator(
            eb.StringAtom($"{item.Cap.Term.Name}<{item.Cap.Value}"),
            eb.Op(ItemOperatorProvider.Conditional),
            Convert(new MultiItem(item.Name, item.Effects)));

        public static IExpression<ItemExpressionType> Convert(BranchedItem item)
        {
            bool t = TryConvertToItemExpression(item.TrueItem, out IExpression<ItemExpressionType> exprT);
            bool f = TryConvertToItemExpression(item.FalseItem, out IExpression<ItemExpressionType> exprF);
            IExpression<ItemExpressionType> logic = eb.StringAtom(item.Logic.InfixSource);
            OperatorToken op = eb.Op(ItemOperatorProvider.Conditional);

            if (t && !f)
            {
                return eb.ApplyInfixOperator(logic, op, exprT);
            }
            else if (!t && f)
            {
                return eb.ApplyInfixOperator(eb.ApplyPrefixOperator(eb.Op(ItemOperatorProvider.Negation), logic), op, exprF);
            }
            else if (t && f)
            {
                return eb.ApplyInfixOperator(
                    eb.ApplyInfixOperator(logic, op, exprT),
                    eb.Op(ItemOperatorProvider.Chaining),
                    eb.ApplyInfixOperator(eb.ApplyPrefixOperator(eb.Op(ItemOperatorProvider.Negation), logic), op, exprF)
                    );
            }
            else
            {
                return eb.ApplyInfixOperator(logic, op, eb.NameAtom(ItemExpressionFactory.EmptyEffect));
            }
        }

        public static bool TryConvertToItemExpression(LogicItem item, out IExpression<ItemExpressionType> expr)
        {
            switch (item)
            {
                case SingleItem si:
                    expr = Convert(si);
                    return true;
                case MultiItem mi:
                    expr = Convert(mi);
                    return true;
                case EmptyItem ei:
                    expr = Convert(ei);
                    return true;
                case BoolItem bi:
                    expr = Convert(bi);
                    return true;
                case CappedItem ci:
                    expr = Convert(ci);
                    return true;
                case BranchedItem bi:
                    expr = Convert(bi);
                    return true;
                case null:
                default:
                    expr = default;
                    return false;
            }
        }
    }
}

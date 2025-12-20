using RandomizerCore.StringParsing;

namespace RandomizerCore.StringItems
{
    public enum ItemExpressionType
    {
        Int,
        Bool,
        TermLike,
        ItemEffect
    }

    public record ItemAtomExpression(Token Token) : AtomExpression<ItemExpressionType>(Token)
    {
        public override IEnumerable<ItemExpressionType> SpeculateType() => Token switch
        {
            NameToken => [ItemExpressionType.TermLike, ItemExpressionType.Bool],
            NumberToken => [ItemExpressionType.Int],
            StringToken => [ItemExpressionType.Bool],
            _ => throw new NotImplementedException()
        };

        protected internal override bool Validate(ExpressionValidator<ItemExpressionType> validator, int Offset) => 
            validator.Expect(this, Offset,
                () => Token is NameToken or NumberToken or StringToken, 
                "Expected a symbol name, number, or logic string literal."
            );
    }

    public record EmptyEffectExpression(Token Token) : AtomExpression<ItemExpressionType>(Token)
    {
        public override IEnumerable<ItemExpressionType> SpeculateType() => [ItemExpressionType.ItemEffect];

        protected internal override bool Validate(ExpressionValidator<ItemExpressionType> validator, int Offset) =>
            validator.Expect(this, Offset,
                () => Token is NameToken nt && nt.Content == "_",
                "Expected exactly \"_\"."
            );
    }

    // ----- prefix expressions ----- //

    public record NegationExpression(OperatorToken Operator, Expression<ItemExpressionType> Operand) 
        : PrefixExpression<ItemExpressionType>(Operator, Operand)
    {
        public override IEnumerable<ItemExpressionType> SpeculateType() => [ItemExpressionType.Bool];

        protected internal override bool Validate(ExpressionValidator<ItemExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, ItemOperatorProvider.Negation),
            () => validator.ExpectAllSequential(
                () => Operand.Validate(validator, 1 + Offset),
                () => validator.ExpectType(Operand, 1 + Offset, ItemExpressionType.Bool)
            )
        );
    }

    public record ReferenceExpression(OperatorToken Operator, Expression<ItemExpressionType> Operand)
        : PrefixExpression<ItemExpressionType>(Operator, Operand)
    {
        public override IEnumerable<ItemExpressionType> SpeculateType() => [ItemExpressionType.ItemEffect, ItemExpressionType.Bool];

        protected internal override bool Validate(ExpressionValidator<ItemExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, ItemOperatorProvider.Reference),
            () => validator.ExpectAllSequential(
                () => Operand.Validate(validator, 1 + Offset),
                () => validator.ExpectType(Operand, 1 + Offset, ItemExpressionType.TermLike)
            )
        );
    }

    // ----- postfix expressions ----- //

    public record CoalescingExpression(Expression<ItemExpressionType> Operand, OperatorToken Operator) 
        : PostfixExpression<ItemExpressionType>(Operand, Operator)
    {
        public override IEnumerable<ItemExpressionType> SpeculateType() => [ItemExpressionType.TermLike];

        protected internal override bool Validate(ExpressionValidator<ItemExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, ItemOperatorProvider.TermCoalescing),
            () => validator.ExpectAllSequential(
                () => Operand.Validate(validator, Offset),
                () => validator.ExpectType(Operand, Offset, ItemExpressionType.TermLike)
            )
        );
    }

    public record IncrementExpression(Expression<ItemExpressionType> Operand, OperatorToken Operator)
        : PostfixExpression<ItemExpressionType>(Operand, Operator)
    {
        public override IEnumerable<ItemExpressionType> SpeculateType() => [ItemExpressionType.ItemEffect];

        protected internal override bool Validate(ExpressionValidator<ItemExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, ItemOperatorProvider.Increment),
            () => validator.ExpectAllSequential(
                () => Operand.Validate(validator, Offset),
                () => validator.ExpectType(Operand, Offset, ItemExpressionType.TermLike)
            )
        );
    }

    // ----- infix expressions ----- //

    public record AdditionAssignmentExpression(Expression<ItemExpressionType> Left, OperatorToken Operator, Expression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> SpeculateType() => [ItemExpressionType.ItemEffect];

        protected internal override bool Validate(ExpressionValidator<ItemExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, ItemOperatorProvider.AdditionAssignment),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator, Offset),
                () => validator.ExpectType(Left, Offset, ItemExpressionType.TermLike)
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator, Offset + Left.TokenCount + 1),
                () => validator.ExpectType(Right, Offset + Left.TokenCount + 1, ItemExpressionType.Int)
            )
        );
    }

    public record MaxAssignmentExpression(Expression<ItemExpressionType> Left, OperatorToken Operator, Expression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> SpeculateType() => [ItemExpressionType.ItemEffect];

        protected internal override bool Validate(ExpressionValidator<ItemExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, ItemOperatorProvider.MaxAssignment),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator, Offset),
                () => validator.ExpectType(Left, Offset, ItemExpressionType.TermLike)
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator, Offset + Left.TokenCount + 1),
                () => validator.ExpectType(Right, Offset + Left.TokenCount + 1, ItemExpressionType.Int)
            )
        );
    }

    public record ConditionalExpression(Expression<ItemExpressionType> Left, OperatorToken Operator, Expression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> SpeculateType() => [ItemExpressionType.ItemEffect];

        protected internal override bool Validate(ExpressionValidator<ItemExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, ItemOperatorProvider.Conditional),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator, Offset),
                () => validator.ExpectType(Left, Offset, ItemExpressionType.Bool)
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator, Offset + Left.TokenCount + 1),
                () => validator.ExpectType(Right, Offset + Left.TokenCount + 1, ItemExpressionType.ItemEffect)
            )
        );
    }

    public record ShortCircuitChainingExpression(Expression<ItemExpressionType> Left, OperatorToken Operator, Expression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> SpeculateType() => [ItemExpressionType.ItemEffect];

        protected internal override bool Validate(ExpressionValidator<ItemExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, ItemOperatorProvider.ShortCircuitChaining),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator, Offset),
                () => validator.ExpectType(Left, Offset, ItemExpressionType.ItemEffect)
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator, Offset + Left.TokenCount + 1),
                () => validator.ExpectType(Right, Offset + Left.TokenCount + 1, ItemExpressionType.ItemEffect)
            )
        );
    }

    public record ChainingExpression(Expression<ItemExpressionType> Left, OperatorToken Operator, Expression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> SpeculateType() => [ItemExpressionType.ItemEffect];

        protected internal override bool Validate(ExpressionValidator<ItemExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, ItemOperatorProvider.Chaining),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator, Offset),
                () => validator.ExpectType(Left, Offset, ItemExpressionType.ItemEffect)
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator, Offset + Left.TokenCount + 1),
                () => validator.ExpectType(Right, Offset + Left.TokenCount + 1, ItemExpressionType.ItemEffect)
            )
        );
    }
}

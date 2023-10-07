using RandomizerCore.StringParsing;

namespace RandomizerCore.StringItem
{
    public enum ItemExpressionType
    {
        Int,
        Bool,
        TermLike,
        ItemEffect
    }

    public record ItemAtomExpression(Token Token) : IExpression<ItemExpressionType>
    {
        public int StartChar => Token.StartCharacter;
        public int EndChar => Token.EndCharacter;

        public IEnumerable<ItemExpressionType> Evaluate() => Token switch
        {
            NameToken => new[] { ItemExpressionType.TermLike, ItemExpressionType.Bool },
            NumberToken => new[] { ItemExpressionType.Int },
            StringToken => new[] { ItemExpressionType.Bool },
            _ => throw new NotImplementedException()
        };

        public bool Validate(ExpressionValidator<ItemExpressionType> validator) => 
            validator.Expect(
                () => Token is NameToken or NumberToken or StringToken, 
                Token.StartCharacter, Token.EndCharacter,
                "Expected a symbol name, number, or logic string literal."
            );

        public string Print() => Token.Print();

        public static bool IsAtomToken(Token token) => new ItemAtomExpression(token).Validate(new());
    }

    // ----- prefix expressions ----- //

    public record NegationExpression(OperatorToken Operator, IExpression<ItemExpressionType> Operand) 
        : PrefixExpression<ItemExpressionType>(Operator, Operand)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.Bool };

        public override bool Validate(ExpressionValidator<ItemExpressionType> validator) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(Operator, ItemOperatorProvider.Negation),
            () => validator.ExpectAllSequential(
                () => Operand.Validate(validator),
                () => validator.ExpectType(Operand, ItemExpressionType.Bool)
            )
        );
    }

    public record ReferenceExpression(OperatorToken Operator, IExpression<ItemExpressionType> Operand)
        : PrefixExpression<ItemExpressionType>(Operator, Operand)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect, ItemExpressionType.Bool };

        public override bool Validate(ExpressionValidator<ItemExpressionType> validator) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(Operator, ItemOperatorProvider.Reference),
            () => validator.ExpectAllSequential(
                () => Operand.Validate(validator),
                () => validator.ExpectType(Operand, ItemExpressionType.TermLike)
            )
        );
    }

    // ----- postfix expressions ----- //

    public record CoalescingExpression(IExpression<ItemExpressionType> Operand, OperatorToken Operator) 
        : PostfixExpression<ItemExpressionType>(Operand, Operator)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.TermLike };

        public override bool Validate(ExpressionValidator<ItemExpressionType> validator) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(Operator, ItemOperatorProvider.TermCoalescing),
            () => validator.ExpectAllSequential(
                () => Operand.Validate(validator),
                () => validator.ExpectType(Operand, ItemExpressionType.TermLike)
            )
        );
    }

    public record IncrementExpression(IExpression<ItemExpressionType> Operand, OperatorToken Operator)
        : PostfixExpression<ItemExpressionType>(Operand, Operator)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate(ExpressionValidator<ItemExpressionType> validator) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(Operator, ItemOperatorProvider.Increment),
            () => validator.ExpectAllSequential(
                () => Operand.Validate(validator),
                () => validator.ExpectType(Operand, ItemExpressionType.TermLike)
            )
        );
    }

    // ----- infix expressions ----- //

    public record AdditionAssignmentExpression(IExpression<ItemExpressionType> Left, OperatorToken Operator, IExpression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate(ExpressionValidator<ItemExpressionType> validator) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(Operator, ItemOperatorProvider.AdditionAssignment),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator),
                () => validator.ExpectType(Left, ItemExpressionType.TermLike)
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator),
                () => validator.ExpectType(Right, ItemExpressionType.Int)
            )
        );
    }

    public record MaxAssignmentExpression(IExpression<ItemExpressionType> Left, OperatorToken Operator, IExpression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate(ExpressionValidator<ItemExpressionType> validator) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(Operator, ItemOperatorProvider.MaxAssignment),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator),
                () => validator.ExpectType(Left, ItemExpressionType.TermLike)
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator),
                () => validator.ExpectType(Right, ItemExpressionType.Int)
            )
        );
    }

    public record ConditionalExpression(IExpression<ItemExpressionType> Left, OperatorToken Operator, IExpression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate(ExpressionValidator<ItemExpressionType> validator) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(Operator, ItemOperatorProvider.Conditional),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator),
                () => validator.ExpectType(Left, ItemExpressionType.Bool)
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator),
                () => validator.ExpectType(Right, ItemExpressionType.ItemEffect)
            )
        );
    }

    public record ShortCircuitChainingExpression(IExpression<ItemExpressionType> Left, OperatorToken Operator, IExpression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate(ExpressionValidator<ItemExpressionType> validator) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(Operator, ItemOperatorProvider.ShortCircuitChaining),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator),
                () => validator.ExpectType(Left, ItemExpressionType.ItemEffect)
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator),
                () => validator.ExpectType(Right, ItemExpressionType.ItemEffect)
            )
        );
    }

    public record ChainingExpression(IExpression<ItemExpressionType> Left, OperatorToken Operator, IExpression<ItemExpressionType> Right) 
        : InfixExpression<ItemExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<ItemExpressionType> Evaluate() => new[] { ItemExpressionType.ItemEffect };

        public override bool Validate(ExpressionValidator<ItemExpressionType> validator) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(Operator, ItemOperatorProvider.Chaining),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator),
                () => validator.ExpectType(Left, ItemExpressionType.ItemEffect)
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator),
                () => validator.ExpectType(Right, ItemExpressionType.ItemEffect)
            )
        );
    }
}

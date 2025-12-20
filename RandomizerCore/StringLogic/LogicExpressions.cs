using RandomizerCore.StringParsing;
using Op = RandomizerCore.StringParsing.OperatorToken;

namespace RandomizerCore.StringLogic
{
    public enum LogicExpressionType
    {
        /// <summary>
        /// The identifier of a term or logic variable (including numeric literals).
        /// </summary>
        TermLike,
        /// <summary>
        /// A logic expression without a state provider. May be composed of state modifiers or any operations which read progresison terms.
        /// </summary>
        Modifier,
        /// <summary>
        /// A logic expression with a state provider. May be contain additional state modifiers or any operations which read progresison terms.
        /// </summary>
        State,
    }

    public record LogicAtomExpression(Token Token) : AtomExpression<LogicExpressionType>(Token)
    {
        public override IEnumerable<LogicExpressionType> SpeculateType() => Token switch
        {
            NameToken => [LogicExpressionType.TermLike, LogicExpressionType.Modifier, LogicExpressionType.State],
            _ => throw new NotImplementedException(),
        };

        protected internal override bool Validate(ExpressionValidator<LogicExpressionType> validator, int Offset) => validator.Expect(this, Offset,
            () => Token is NameToken,
                "Expected a symbol name");
    }

    public record NumberLiteralExpression(Token Token) : AtomExpression<LogicExpressionType>(Token)
    {
        public int ConstValue => ((NumberToken)Token).Value;

        public override IEnumerable<LogicExpressionType> SpeculateType() => Token switch
        {
            NumberToken => [LogicExpressionType.TermLike, LogicExpressionType.Modifier],
            _ => throw new NotImplementedException(),
        };

        protected internal override bool Validate(ExpressionValidator<LogicExpressionType> validator, int Offset) => validator.Expect(this, Offset,
            () => Token is NumberToken,
                "Expected a numeric literal");
    }

    public record BoolLiteralExpression(Token Token) : AtomExpression<LogicExpressionType>(Token)
    {
        public const string NONE = "NONE";
        public const string FALSE = "FALSE";
        public const string ANY = "ANY";
        public const string TRUE = "TRUE";

        public static bool IsConstAtom(string s) => s switch
        {
            NONE or FALSE or ANY or TRUE => true,
            _ => false
        };

        public bool ConstValue => Token.Content switch
        {
            "FALSE" or "NONE" => false,
            "TRUE" or "ANY" => true,
            _ => throw new NotImplementedException(Token.Content),
        };

        public override IEnumerable<LogicExpressionType> SpeculateType()
        {
            if (ConstValue)
            {
                return [LogicExpressionType.TermLike, LogicExpressionType.Modifier];
            }
            else
            {
                return [LogicExpressionType.TermLike, LogicExpressionType.Modifier, LogicExpressionType.State]; // NONE can replace a state provider
            }
        }

        protected internal override bool Validate(ExpressionValidator<LogicExpressionType> validator, int Offset) => validator.Expect(this, Offset,
            () => Token is NameToken && IsConstAtom(Token.Content), "Expected one of NONE, FALSE, ANY, TRUE");
    }

    /// <summary>
    /// An expression which is always undefined, for the purpose of the coalescing operator.
    /// Produces an error if present when expression is interpreted.
    /// </summary>
    public record NullLiteralExpression(Token Token) : AtomExpression<LogicExpressionType>(Token)
    {
        public const string NULL = "NULL";

        public static bool IsNullAtom(string n) => n == NULL;

        public override IEnumerable<LogicExpressionType> SpeculateType() => [LogicExpressionType.TermLike, LogicExpressionType.Modifier, LogicExpressionType.State];

        protected internal override bool Validate(ExpressionValidator<LogicExpressionType> validator, int Offset) => validator.Expect(this, Offset,
            () => Token is NameToken && IsNullAtom(Token.Content), "Expected NULL");
    }

    // ----- prefix expressions ----- //

    public record ReferenceExpression(Op Operator, Expression<LogicExpressionType> Operand)
        : PrefixExpression<LogicExpressionType>(Operator, Operand)
    {
        public override IEnumerable<LogicExpressionType> SpeculateType() => [LogicExpressionType.State, LogicExpressionType.Modifier];

        protected internal override bool Validate(ExpressionValidator<LogicExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, LogicOperatorProvider.REF),
            () => validator.ExpectAllSequential(
                () => Operand.Validate(validator, 1 + Offset),
                () => validator.ExpectType(Operand, 1 + Offset, LogicExpressionType.TermLike)
                )
            );
    }

    // ----- postfix expressions ----- //

    public record ProjectionExpression(Expression<LogicExpressionType> Operand, Op Operator)
        : PostfixExpression<LogicExpressionType>(Operand, Operator)
    {
        public override IEnumerable<LogicExpressionType> SpeculateType() => [LogicExpressionType.Modifier];

        protected internal override bool Validate(ExpressionValidator<LogicExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectAllSequential(
                () => Operand.Validate(validator, Offset),
                () => validator.ExpectType(Operand, Offset, LogicExpressionType.State)
                ),
            () => validator.ExpectOperator(this, Offset, Operator, LogicOperatorProvider.PROJECT)
            );
    }

    // ----- infix expressions ----- //

    public record AndExpression(Expression<LogicExpressionType> Left, Op Operator, Expression<LogicExpressionType> Right)
        : InfixExpression<LogicExpressionType>(Left, Operator, Right)
    {
        private static readonly Dictionary<(LogicExpressionType, LogicExpressionType), LogicExpressionType> TypeMap = new()
        {
            [(LogicExpressionType.State, LogicExpressionType.Modifier)] = LogicExpressionType.State,
            [(LogicExpressionType.Modifier, LogicExpressionType.Modifier)] = LogicExpressionType.Modifier
        };

        public override IEnumerable<LogicExpressionType> SpeculateType()
        {
            return Util.PartialZip(Left.SpeculateType(), Right.SpeculateType(), TypeMap);
        }

        protected internal override bool Validate(ExpressionValidator<LogicExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, LogicOperatorProvider.AND),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator, Offset),
                () => validator.ExpectTypes(Left, Offset, TypeMap.Keys.Select(k => k.Item1).ToArray())
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator, Offset + Left.TokenCount + 1),
                () => validator.ExpectTypes(Right, Offset + Left.TokenCount + 1, TypeMap.Keys.Select(k => k.Item2).ToArray())
            )
        );
    }

    public record OrExpression(Expression<LogicExpressionType> Left, Op Operator, Expression<LogicExpressionType> Right)
        : InfixExpression<LogicExpressionType>(Left, Operator, Right)
    {
        private static readonly Dictionary<(LogicExpressionType, LogicExpressionType), LogicExpressionType> TypeMap = new()
        {
            [(LogicExpressionType.State, LogicExpressionType.State)] = LogicExpressionType.State,
            [(LogicExpressionType.Modifier, LogicExpressionType.Modifier)] = LogicExpressionType.Modifier
        };

        public override IEnumerable<LogicExpressionType> SpeculateType()
        {
            return Util.PartialZip(Left.SpeculateType(), Right.SpeculateType(), TypeMap);
        }

        protected internal override bool Validate(ExpressionValidator<LogicExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, LogicOperatorProvider.OR),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator, Offset),
                () => validator.ExpectTypes(Left, Offset, TypeMap.Keys.Select(k => k.Item1).ToArray())
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator, Offset + Left.TokenCount + 1),
                () => validator.ExpectTypes(Right, Offset + Left.TokenCount + 1, TypeMap.Keys.Select(k => k.Item2).ToArray())
            )
        );
    }

    public record CoalesceExpression(Expression<LogicExpressionType> Left, Op Operator, Expression<LogicExpressionType> Right)
        : InfixExpression<LogicExpressionType>(Left, Operator, Right)
    {
        private static readonly Dictionary<(LogicExpressionType, LogicExpressionType), LogicExpressionType> TypeMap = new()
        {
            [(LogicExpressionType.State, LogicExpressionType.State)] = LogicExpressionType.State,
            [(LogicExpressionType.Modifier, LogicExpressionType.Modifier)] = LogicExpressionType.Modifier,
            [(LogicExpressionType.TermLike, LogicExpressionType.TermLike)] = LogicExpressionType.TermLike
        };

        public override IEnumerable<LogicExpressionType> SpeculateType()
        {
            return Util.PartialZip(Left.SpeculateType(), Right.SpeculateType(), TypeMap);
        }

        protected internal override bool Validate(ExpressionValidator<LogicExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperator(this, Offset, Operator, LogicOperatorProvider.COALESCE),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator, Offset),
                () => validator.ExpectTypes(Left, Offset, TypeMap.Keys.Select(k => k.Item1).ToArray())
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator, Offset + Left.TokenCount + 1),
                () => validator.ExpectTypes(Right, Offset + Left.TokenCount + 1, TypeMap.Keys.Select(k => k.Item2).ToArray())
            )
        );
    }

    public record ComparisonExpression(Expression<LogicExpressionType> Left, Op Operator, Expression<LogicExpressionType> Right)
        : InfixExpression<LogicExpressionType>(Left, Operator, Right)
    {
        public override IEnumerable<LogicExpressionType> SpeculateType() => [LogicExpressionType.Modifier];

        protected internal override bool Validate(ExpressionValidator<LogicExpressionType> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.ExpectOperators(this, Offset, Operator, [LogicOperatorProvider.LT, LogicOperatorProvider.GT, LogicOperatorProvider.EQ]),
            () => validator.ExpectAllSequential(
                () => Left.Validate(validator, Offset),
                () => validator.ExpectType(Left, Offset, LogicExpressionType.TermLike)
            ),
            () => validator.ExpectAllSequential(
                () => Right.Validate(validator, Offset + Left.TokenCount + 1),
                () => validator.ExpectType(Right, Offset + Left.TokenCount + 1, LogicExpressionType.TermLike)
            )
        );
    }

}

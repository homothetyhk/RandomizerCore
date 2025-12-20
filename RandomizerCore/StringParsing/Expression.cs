using System;

namespace RandomizerCore.StringParsing
{
    /// <summary>
    /// An expression with results constrained by the set T
    /// </summary>
    /// <typeparam name="T">The set of valid types for the expression (usually an enum).</typeparam>
    public abstract record Expression<T>
    {
        internal Expression()
        {
            TokenCount = CountTokens();
        }

        /// <summary>
        /// Speculates possible types for an expression. If the result { T.A, T.B } is returned, then the result
        /// is to be interpreted as "A or B".
        /// </summary>
        public abstract IEnumerable<T> SpeculateType();

        /// <summary>
        /// Validates whether the expression is structurally and semantically valid and aggregates any errors
        /// using the provided expression validator.
        /// </summary>
        /// <param name="validator">The validator to use to aggregate errors.</param>
        /// <param name="Offset">The start index of the current expression, relative to the root expression being validated.</param>
        /// <returns>Whether the expression is valid or not.</returns>
        protected internal abstract bool Validate(ExpressionValidator<T> validator, int Offset);

        /// <summary>
        /// Validates the expression, returning true if validation was successful, and outputting a validator which can be queried for error information.
        /// </summary>
        public bool Validate(out ExpressionValidator<T> validator)
        {
            validator = new(this);
            return validator.ValidatedSuccessfully;
        }

        /// <summary>
        /// Enumerates the direct subexpressions of the expression (nonrecursive, not including self).
        /// </summary>
        public abstract IEnumerable<Expression<T>> GetChildren();
        
        /// <summary>
        /// A precomputed count of the underlying tokens that make up the expression.
        /// </summary>
        public int TokenCount { get; }
        
        /// <summary>
        /// Enumerates the underlying tokens that make up the expression, from left to right.
        /// </summary>
        public abstract IEnumerable<Token> GetTokens();
        
        /// <summary>
        /// Returns the nth underlying token of the expression.
        /// </summary>
        public abstract Token GetToken(int index);
        
        /// <summary>
        /// Returns the smallest subexpression containing the specified range of tokens.
        /// </summary>
        public abstract Expression<T> GetContainingExpression(int startInc, int endExc);
        
        /// <summary>
        /// Used to precompute <see cref="TokenCount"/>. By default, counts the result of <see cref="GetTokens"/>, but can be overriden to be more efficient.
        /// </summary>
        protected virtual int CountTokens() => GetTokens().Count();

        /// <summary>
        /// Prints the expression with default spacing rules.
        /// </summary>
        public abstract void Print(TextWriter tw);

        /// <summary>
        /// Prints the expression with default spacing rules.
        /// </summary>
        public string Print()
        {
            StringWriter sw = new();
            Print(sw);
            return sw.ToString();
        }
    }

    /// <summary>
    /// A parenthesized nested expression.
    /// </summary>
    /// <param name="OpenParenthesis">The open parenthesis token</param>
    /// <param name="Nested">The nested expression</param>
    /// <param name="CloseParenthesis">The close parenthesis token</param>
    public record GroupingExpression<T>(StructuralToken OpenParenthesis, Expression<T> Nested, StructuralToken CloseParenthesis) : Expression<T>
    {
        /// <inheritdoc/>
        public override IEnumerable<T> SpeculateType() => Nested.SpeculateType();

        /// <inheritdoc/>
        protected internal override bool Validate(ExpressionValidator<T> validator, int Offset) => validator.ExpectAllParallel(
            () => validator.Expect(this, Offset,
                () => OpenParenthesis.Type == StructuralToken.Types.OpenParenthesis,
                "Expected an open parenthesis '('."
            ),
            () => validator.Expect(this, Offset,
                () => CloseParenthesis.Type == StructuralToken.Types.CloseParenthesis,
                "Expected a close parenthesis ')'."
            ),
            () => Nested.Validate(validator, 1 + Offset)
        );

        public sealed override IEnumerable<Expression<T>> GetChildren()
        {
            return [Nested];
        }

        public sealed override IEnumerable<Token> GetTokens()
        {
            yield return OpenParenthesis;
            foreach (Token t in Nested.GetTokens()) yield return t;
            yield return CloseParenthesis;
        }

        protected sealed override int CountTokens() => 1 + Nested.TokenCount + 1;

        public sealed override Token GetToken(int index)
        {
            if (index < 0 || index >= TokenCount) throw new ArgumentOutOfRangeException(nameof(index));
            else if (index == 0) return OpenParenthesis;
            else if (index ==  TokenCount - 1) return CloseParenthesis;
            else return Nested.GetToken(index - 1);
        }

        public sealed override Expression<T> GetContainingExpression(int startInc, int endExc)
        {
            if (startInc < 0) throw new ArgumentOutOfRangeException(nameof(startInc));
            else if (endExc > TokenCount || endExc <= startInc) throw new ArgumentOutOfRangeException(nameof(endExc));
            else if (startInc == 0 || endExc == TokenCount) return this;
            else return Nested.GetContainingExpression(startInc - 1, endExc - 1);
        }

        public sealed override void Print(TextWriter tw)
        {
            tw.Write(OpenParenthesis.Content);
            Nested.Print(tw);
            tw.Write(CloseParenthesis.Content);
        }
    }

    /// <summary>
    /// A prefix expression (prefix operator applied to a nested expression)
    /// </summary>
    /// <param name="Operator">The operator token</param>
    /// <param name="Operand">The operand expression</param>
    public abstract record PrefixExpression<T>(OperatorToken Operator, Expression<T> Operand) : Expression<T>
    {
        public OperatorToken Operator { get; init; } = Operator.Definition.IsPrefix ? 
            Operator : throw new ArgumentException($"{Operator.Definition} is not a prefix operator.");

        public sealed override IEnumerable<Expression<T>> GetChildren()
        {
            return [Operand];
        }

        public sealed override IEnumerable<Token> GetTokens()
        {
            yield return Operator;
            foreach (Token t in Operand.GetTokens()) yield return t;
        }

        public sealed override Token GetToken(int index)
        {
            if (index < 0 || index >= TokenCount) throw new ArgumentOutOfRangeException(nameof(index));
            else if (index == 0) return Operator;
            else return Operand.GetToken(index - 1);
        }

        protected sealed override int CountTokens()
        {
            return 1 + Operand.TokenCount;
        }

        public sealed override Expression<T> GetContainingExpression(int startInc, int endExc)
        {
            if (startInc < 0) throw new ArgumentOutOfRangeException(nameof(startInc));
            else if (endExc > TokenCount || endExc <= startInc) throw new ArgumentOutOfRangeException(nameof(endExc));
            else if (startInc == 0) return this;
            else return Operand.GetContainingExpression(startInc - 1, endExc - 1);
        }

        public sealed override void Print(TextWriter tw)
        {
            tw.Write(Operator.Definition.DefaultLeadingTrivia);
            tw.Write(Operator.Definition.Operator);
            tw.Write(Operator.Definition.DefaultTrailingTrivia);
            Operand.Print(tw);
        }
    }

    /// <summary>
    /// A postfix expression (postfix operator applied to a nested expression)
    /// </summary>
    /// <param name="Operand">The operand expression</param>
    /// <param name="Operator">The operator token</param>
    public abstract record PostfixExpression<T>(Expression<T> Operand, OperatorToken Operator) : Expression<T>
    {
        public OperatorToken Operator { get; init; } = Operator.Definition.IsPostfix ?
            Operator : throw new ArgumentException($"{Operator.Definition} is not a postfix operator.");

        public sealed override IEnumerable<Expression<T>> GetChildren()
        {
            return [Operand];
        }

        public sealed override IEnumerable<Token> GetTokens()
        {
            foreach (Token t in Operand.GetTokens()) yield return t;
            yield return Operator;
        }

        public sealed override Token GetToken(int index)
        {
            if (index < 0 || index >= TokenCount) throw new ArgumentOutOfRangeException(nameof(index));
            else if (index == TokenCount - 1) return Operator;
            else return Operand.GetToken(index);
        }

        protected sealed override int CountTokens()
        {
            return Operand.TokenCount + 1;
        }

        public sealed override Expression<T> GetContainingExpression(int startInc, int endExc)
        {
            if (startInc < 0) throw new ArgumentOutOfRangeException(nameof(startInc));
            else if (endExc > TokenCount || endExc <= startInc) throw new ArgumentOutOfRangeException(nameof(endExc));
            else if (endExc == TokenCount) return this;
            else return Operand.GetContainingExpression(startInc, endExc);
        }

        public sealed override void Print(TextWriter tw)
        {
            Operand.Print(tw);
            tw.Write(Operator.Definition.DefaultLeadingTrivia);
            tw.Write(Operator.Definition.Operator);
            tw.Write(Operator.Definition.DefaultTrailingTrivia);
        }
    }

    /// <summary>
    /// An infix expression (infix operator applied between 2 nested expressions)
    /// </summary>
    /// <param name="Left">The left-hand operand</param>
    /// <param name="Operator"></param>
    /// <param name="Right">The right-hand operand</param>
    public abstract record InfixExpression<T>(Expression<T> Left, OperatorToken Operator, Expression<T> Right) : Expression<T>
    {
        public OperatorToken Operator { get; init; } = Operator.Definition.IsInfix ?
            Operator : throw new ArgumentException($"{Operator.Definition} is not an infix operator.");

        public sealed override IEnumerable<Expression<T>> GetChildren()
        {
            return [Left, Right];
        }

        public sealed override IEnumerable<Token> GetTokens()
        {
            foreach (Token t in Left.GetTokens()) yield return t;
            yield return Operator;
            foreach (Token t in Right.GetTokens()) yield return t;
        }

        public sealed override Token GetToken(int index)
        {
            if (index < 0 || index >= TokenCount) throw new ArgumentOutOfRangeException(nameof(index));
            else if (index < Left.TokenCount) return Left.GetToken(index);
            else if (index == Left.TokenCount) return Operator;
            else return Right.GetToken(index - Left.TokenCount - 1);
        }

        protected sealed override int CountTokens()
        {
            return Left.TokenCount + 1 + Right.TokenCount;
        }

        public sealed override Expression<T> GetContainingExpression(int startInc, int endExc)
        {
            if (startInc < 0) throw new ArgumentOutOfRangeException(nameof(startInc));
            else if (endExc > TokenCount || endExc <= startInc) throw new ArgumentOutOfRangeException(nameof(endExc));
            else if (endExc <= Left.TokenCount) return Left.GetContainingExpression(startInc, endExc);
            else if (startInc > Left.TokenCount + 1) return Right.GetContainingExpression(startInc - Left.TokenCount - 1, endExc - Left.TokenCount - 1);
            else return this;
        }

        /// <summary>
        /// For the given associative infix expression type, recursively unfolds that type and any intermediate grouping expressions, returning a sequence of expressions on which the infix operator acts left to right.
        /// </summary>
        public static IEnumerable<Expression<T>> FlattenAssoc<U>(U u) where U : InfixExpression<T>
        {
            return FlattenAssocHelper<U>(u.Left).Concat(FlattenAssocHelper<U>(u.Right));
        }

        private static IEnumerable<Expression<T>> FlattenAssocHelper<U>(Expression<T> e) where U : InfixExpression<T>
        {
            if (e is GroupingExpression<T> g) return FlattenAssocHelper<U>(g.Nested);
            else if (e is U u) return FlattenAssoc(u);
            else return [e];
        }

        public sealed override void Print(TextWriter tw)
        {
            Left.Print(tw);
            tw.Write(Operator.Definition.DefaultLeadingTrivia);
            tw.Write(Operator.Definition.Operator);
            tw.Write(Operator.Definition.DefaultTrailingTrivia);
            Right.Print(tw);
        }
    }

    /// <summary>
    /// An expression consisting of a single nonoperator token.
    /// </summary>
    /// <param name="Token"></param>
    public abstract record AtomExpression<T>(Token Token) : Expression<T>
    {
        public Token Token { get; init; } = Token switch 
        { StructuralToken or OperatorToken => throw new ArgumentException($"Token {Token} is not an atom token"), _ => Token };

        public sealed override IEnumerable<Expression<T>> GetChildren()
        {
            return [];
        }

        public sealed override IEnumerable<Token> GetTokens()
        {
            return [Token];
        }

        public sealed override Token GetToken(int index)
        {
            return index == 0 ? Token : throw new ArgumentOutOfRangeException(nameof(index));
        }

        protected sealed override int CountTokens()
        {
            return 1;
        }

        public sealed override Expression<T> GetContainingExpression(int startInc, int endExc)
        {
            if (startInc < 0) throw new ArgumentOutOfRangeException(nameof(startInc));
            if (endExc > 1 || endExc <= startInc) throw new ArgumentOutOfRangeException(nameof(endExc));
            return this;
        }

        public override void Print(TextWriter tw)
        {
            tw.Write(Token.Print());
        }
    }
}

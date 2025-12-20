namespace RandomizerCore.StringParsing
{
    /// <summary>
    /// Provides typing validation and error aggregation for <see cref="Expression{T}"/>s
    /// </summary>
    public class ExpressionValidator<T>
    {
        private readonly List<string> errors = [];
        public IReadOnlyList<string> Errors => errors;
        public bool ValidatedSuccessfully { get; }

        /// <summary>
        /// The root expression being validated.
        /// </summary>
        public Expression<T> Root { get; }

        /// <summary>
        /// Creates an object carrying the validation info for the expression. Check <see cref="Errors"/> for aggregated validation errors.
        /// </summary>
        public ExpressionValidator(Expression<T> expression)
        {
            Root = expression;
            ValidatedSuccessfully = Root.Validate(this, 0);
        }

        /// <summary>
        /// Adds an error message to be aggregated in <see cref="Errors"/>. Prepends with the token range of the expression in which the error was found.
        /// </summary>
        private void AddError(int start, int end, string message)
        {
            errors.Add($"[{start}:{end}] {message}");
        }

        /// <summary>
        /// Assert/validate a custom condition holds.
        /// </summary>
        /// <param name="expression">The expression to check</param>
        /// <param name="Offset">The index of the expression relative to the root expression being validated.</param>
        /// <param name="predicate">The condition to evaluate</param>
        /// <param name="message">The error message</param>
        /// <returns>Whether the condition was met (i.e. the result of predicate)</returns>
        public bool Expect(Expression<T> expression, int Offset, Func<bool> predicate, string message)
        {
            if (!predicate())
            {
                AddError(Offset, Offset + expression.TokenCount, message);
                return false;
            }
            return true;
        }

        /// <summary>
        /// Assert/validate that the expression has any speculated type.
        /// </summary>
        /// <param name="expression">The expression to check</param>
        /// <param name="Offset">The index of the expression relative to the root expression being validated.</param>
        /// <returns></returns>
        public bool ExpectAnyType(Expression<T> expression, int Offset)
        {
            IEnumerable<T> possibleTypes = expression.SpeculateType();
            if (!possibleTypes.Any())
            {
                AddError(Offset, Offset + expression.TokenCount,
                    "Expected speculated type list to be nonempty.");
                return false;
            }
            return true;
        }

        /// <summary>
        /// Assert/validate that an expression evaluates to the expected type
        /// </summary>
        /// <param name="expression">The expression to check</param>
        /// <param name="Offset">The index of the expression relative to the root expression being validated.</param>
        /// <param name="expectedType">The expected type</param>
        public bool ExpectType(Expression<T> expression, int Offset, T expectedType)
        {
            IEnumerable<T> possibleTypes = expression.SpeculateType();
            if (!possibleTypes.Contains(expectedType))
            {
                string actualType;
                if (possibleTypes.Count() == 1)
                {
                    actualType = $"type {possibleTypes.First()}";
                }
                else
                {
                    actualType = $"any of the following types: {string.Join(", ", possibleTypes)}";
                }
                AddError(Offset, Offset + expression.TokenCount, 
                    $"Expected an expression of type {expectedType} but got one of {actualType}.");
                return false;
            }
            return true;
        }

        /// <summary>
        /// Assert/validate that an expression evaluates to one of the expected types.
        /// </summary>
        /// <param name="expression">The expression to check</param>
        /// <param name="Offset">The index of the expression relative to the root expression being validated.</param>
        /// <param name="expectedTypes">The expected types</param>
        public bool ExpectTypes(Expression<T> expression, int Offset, T[] expectedTypes)
        {
            T[] possibleTypes = expression.SpeculateType().ToArray();
            if (!possibleTypes.Intersect(expectedTypes).Any())
            {
                string actualType;
                
                if (possibleTypes.Length == 1)
                {
                    actualType = $"type {possibleTypes[0]}";
                }
                else
                {
                    actualType = $"any of the following types: {string.Join(", ", possibleTypes)}";
                }
                AddError(Offset, Offset + expression.TokenCount,
                    $"Expected an expression of any of the following types: {string.Join(", ", expectedTypes)}. Got one of {actualType}.");
                return false;
            }
            return true;
        }

        /// <summary>
        /// Assert/validate that the correct operator was used
        /// </summary>
        /// <param name="expression">The expression to check</param>
        /// <param name="Offset">The index of the expression relative to the root expression being validated.</param>
        /// <param name="op">The operator to </param>
        /// <param name="expectedOp">The expected operator</param>
        public bool ExpectOperator(Expression<T> expression, int Offset, OperatorToken op, string expectedOp)
        {
            if (op.Definition.Operator != expectedOp)
            {
                AddError(Offset, Offset + expression.TokenCount, $"Expected the operator '{expectedOp}' but got '{op.Definition.Operator}'.");
                return false;
            }
            return true;
        }

        /// <summary>
        /// Assert/validate that one of the correct operators was used
        /// </summary>
        /// <param name="expression">The expression to check</param>
        /// <param name="Offset">The index of the expression relative to the root expression being validated.</param>
        /// <param name="op">The operator to </param>
        /// <param name="expectedOps">The expected operator</param>
        public bool ExpectOperators(Expression<T> expression, int Offset, OperatorToken op, string[] expectedOps)
        {
            if (!expectedOps.Contains(op.Definition.Operator))
            {
                AddError(Offset, Offset + expression.TokenCount,
                    $"Expected operator among {string.Join(", ", expectedOps.Select(op => $"'{op}'"))} " +
                    $"but got '{op.Definition.Operator}'.");
                return false;
            }
            return true;
        }

        /// <summary>
        /// Performs a series of expectations in series - if one expectation fails then this expectation will fail and short-circuit.
        /// Useful for expectations dependent on each other, ie given 2 rules A and B, B's error message is not useful if A fails.
        /// </summary>
        /// <param name="predicates">
        /// A group of expectations to evaluate. Technically this can be any producer but it's generally expected
        /// for them to be wrappers for calls to Expect* or calls to <see cref="Expression{T}.Validate(ExpressionValidator{T}, int)"/>
        /// </param>
        public bool ExpectAllSequential(params Func<bool>[] predicates)
        {
            foreach (Func<bool> predicate in predicates)
            {
                if (!predicate())
                {
                    return false;
                }
            }
            return true;
        }

        /// <summary>
        /// Performs multiple expectations in parallel - if one expectation fails then this expectation will fail, but will not short-circuit.
        /// Useful for expectations not dependent on each other, ie given 2 rules A and B, both A and B can independently produce useful error messages.
        /// </summary>
        /// <param name="predicates">
        /// A group of expectations to evaluate. Technically this can be any producer but it's generally expected
        /// for them to be wrappers for calls to Expect*.
        /// </param>
        public bool ExpectAllParallel(params Func<bool>[] predicates)
        {
            bool result = true;
            foreach (Func<bool> predicate in predicates)
            {
                // evaluate the predicate first so we will never short-circuit an expectation
                result = predicate() && result;
            }
            return result;
        }
    }
}

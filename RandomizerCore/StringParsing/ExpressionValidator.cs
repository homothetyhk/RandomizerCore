namespace RandomizerCore.StringParsing
{
    /// <summary>
    /// Provides typing validation and error aggregation for <see cref="IExpression{T}"/>s
    /// </summary>
    public class ExpressionValidator<T>
    {
        private List<string> errors = new();
        public IReadOnlyList<string> Errors => errors;

        private void AddError(int start, int end, string message)
        {
            errors.Add($"[{start}:{end}] {message}");
        }

        /// <summary>
        /// Assert/validate a custom condition holds.
        /// </summary>
        /// <param name="predicate">The condition to evaluate</param>
        /// <param name="startChar">The starting character index where the error is present</param>
        /// <param name="endChar">The ending character index where the error is present</param>
        /// <param name="message">The error message</param>
        /// <returns>Whether the condition was met (i.e. the result of predicate)</returns>
        public bool Expect(Func<bool> predicate, int startChar, int endChar, string message)
        {
            if (!predicate())
            {
                AddError(startChar, endChar, message);
                return false;
            }
            return true;
        }

        /// <summary>
        /// Assert/validate that an expression evaluates to the expected type
        /// </summary>
        /// <param name="expression">The expression to check</param>
        /// <param name="expectedType">The expected type</param>
        public bool ExpectType(IExpression<T> expression, T expectedType)
        {
            IEnumerable<T> possibleTypes = expression.Evaluate();
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
                AddError(expression.StartChar, expression.EndChar, 
                    $"Expected an expression of type {expectedType} but got one of {actualType}.");
                return false;
            }
            return true;
        }

        /// <summary>
        /// Assert/validate that the correct operator was used
        /// </summary>
        /// <param name="op">The operator to </param>
        /// <param name="expectedOp">The expected operator</param>
        public bool ExpectOperator(OperatorToken op, string expectedOp)
        {
            if (op.Operator != expectedOp)
            {
                AddError(op.StartCharacter, op.EndCharacter, $"Expected the operator '{expectedOp}' but got '{op.Operator}'.");
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
        /// for them to be wrappers for calls to Expect* or calls to <see cref="IExpression{T}.Validate"/>
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

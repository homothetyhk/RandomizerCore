using System.ComponentModel.Design.Serialization;
using System.Runtime.CompilerServices;
using System.Text;

namespace RandomizerCore.StringParsing
{
    public static class ExpressionExtensions
    {
        /// <summary>
        /// Returns the first non-grouping subexpression of the given expression.
        /// </summary>
        public static Expression<T> TrimParens<T>(this Expression<T> expr)
        {
            if (expr is GroupingExpression<T> g) return g.Nested.TrimParens();
            else return expr;
        }

        /// <summary>
        /// Recursively numerates the subexpressions in the expression, omitting parens groupings.
        /// Enumeration is depth-first, ordered according to <see cref="Expression{T}.GetChildren"/>.
        /// </summary>
        public static IEnumerable<Expression<T>> Enumerate<T>(this Expression<T> expr)
        {
            expr = expr.TrimParens();
            yield return expr;
            foreach (Expression<T> e in expr.GetChildren().SelectMany(e => e.Enumerate()))
            {
                yield return e;
            }
        }

        /// <summary>
        /// Compares the two tokens. Currently equivalent to record comparison.
        /// </summary>
        public static bool IsEquivalentTo(this Token self, Token other)
        {
            return (self, other) switch
            {
                (NameToken t1, NameToken t2) => t1.Content == t2.Content,
                (NumberToken t1, NumberToken t2) => t1.Value == t2.Value,
                (StringToken t1, StringToken t2) => t1.Content == t2.Content && t1.Delimiter == t2.Delimiter,
                _ => false
            };
        }

        /// <summary>
        /// Returns whether the two expressions are equivalent in the sense of consisting of the same parsed sequence of operators/tokens.
        /// </summary>
        /// <remarks>
        /// Ignores expression type, beyond atom/infix/prefix/postfix.
        /// </remarks>
        public static bool IsEquivalentTo<T>(this Expression<T> self, Expression<T> other)
        {
            self = self.TrimParens();
            other = other.TrimParens();
            switch (self)
            {
                case AtomExpression<T> a1:
                    return other is AtomExpression<T> a2 && a1.Token.IsEquivalentTo(a2.Token);
                case InfixExpression<T> i1:
                    return other is InfixExpression<T> i2
                        && i1.Operator.Definition.Operator == i2.Operator.Definition.Operator
                        && i1.Left.IsEquivalentTo(i2.Left)
                        && i1.Right.IsEquivalentTo(i2.Right);
                case PrefixExpression<T> pre1:
                    return other is PrefixExpression<T> pre2
                        && pre1.Operator.Definition.Operator == pre2.Operator.Definition.Operator
                        && pre1.Operand.IsEquivalentTo(pre2.Operand);
                case PostfixExpression<T> post1:
                    return other is PostfixExpression<T> post2
                        && post1.Operand.IsEquivalentTo(post2.Operand)
                        && post1.Operator.Definition.Operator == post2.Operator.Definition.Operator;
                default: throw new NotImplementedException(self.GetType().Name);
            }
        }

        /// <summary>
        /// Returns true if any subexpression of self is equivalent to other, ignoring outer parens.
        /// </summary>
        public static bool Contains<T>(this Expression<T> self, Expression<T> other)
        {
            other = other.TrimParens();
            return self.Enumerate().Any(e => e.IsEquivalentTo(other));
        }

        /// <summary>
        /// Replaces any subexpression of <paramref name="orig"/> equivalent to <paramref name="toBeReplaced"/> by <paramref name="replacement"/>.
        /// Equivalence is determined using <see cref="IsEquivalentTo{T}(Expression{T}, Expression{T})"/>, 
        /// and substitution is done by the nonrecursive <see cref="Transform{T}(Expression{T}, Func{Expression{T}, ExpressionBuilder{T}, Expression{T}?}, ExpressionBuilder{T})"/>.
        /// </summary>
        public static Expression<T> Subst<T>(this Expression<T> orig, Expression<T> toBeReplaced, Expression<T> replacement, ExpressionBuilder<T> builder)
        {
            return orig.Transform((e, b) => e.IsEquivalentTo(toBeReplaced) ? replacement : null, builder);
        }

        /// <summary>
        /// Transforms the expression recursively according to the delegate. 
        /// <br/>If the delegate returns null on a subexpression, Transform is called on the subexpressions children.
        /// If the delegate returns nonnull, its result replaces the subexpression (recursively updating its new parents).
        /// </summary>
        public static Expression<T> Transform<T>(this Expression<T> orig, Func<Expression<T>, ExpressionBuilder<T>, Expression<T>?> transform, ExpressionBuilder<T> builder)
        {
            if (transform(orig, builder) is Expression<T> result) return result;

            switch (orig)
            {
                case GroupingExpression<T> g:
                    Expression<T> gNested = g.Nested.Transform(transform, builder);
                    return ReferenceEquals(g.Nested, gNested)
                        ? g : gNested; // remove parens if modified; allows builder to determine if they are still needed.

                case AtomExpression<T>: return orig;

                case InfixExpression<T> i:
                    Expression<T> left = i.Left.Transform(transform, builder);
                    Expression<T> right = i.Right.Transform(transform, builder);
                    return ReferenceEquals(i.Left, left) && ReferenceEquals(i.Right, right)
                        ? i : builder.ApplyInfixOperator(left, i.Operator, right);

                case PrefixExpression<T> pre:
                    Expression<T> preOperand = pre.Operand.Transform(transform, builder);
                    return ReferenceEquals(pre.Operand, preOperand)
                        ? pre : builder.ApplyPrefixOperator(pre.Operator, preOperand);

                case PostfixExpression<T> post:
                    Expression<T> postOperand = post.Operand.Transform(transform, builder);
                    return ReferenceEquals(post.Operand, postOperand)
                        ? post : builder.ApplyPostfixOperator(postOperand, post.Operator);

                default: throw new NotImplementedException(orig.GetType().Name);
            }
        }

        /// <summary>
        /// Similar to <see cref="Transform{T}(Expression{T}, Func{Expression{T}, ExpressionBuilder{T}, Expression{T}?}, ExpressionBuilder{T})"/>,
        /// but if the transform succeeds, it is called again on its result recursively until it fails.
        /// </summary>
        public static Expression<T> TransformRecursive<T>(this Expression<T> orig, Func<Expression<T>, ExpressionBuilder<T>, Expression<T>?> transform, ExpressionBuilder<T> builder)
        {
            if (transform(orig, builder) is Expression<T> result) return result.TransformRecursive(transform, builder);

            switch (orig)
            {
                case GroupingExpression<T> g:
                    Expression<T> gNested = g.Nested.TransformRecursive(transform, builder);
                    return ReferenceEquals(g.Nested, gNested)
                        ? g : gNested; // remove parens if modified; allows builder to determine if they are still needed.

                case AtomExpression<T>: return orig;

                case InfixExpression<T> i:
                    Expression<T> left = i.Left.TransformRecursive(transform, builder);
                    Expression<T> right = i.Right.TransformRecursive(transform, builder);
                    return ReferenceEquals(i.Left, left) && ReferenceEquals(i.Right, right)
                        ? i : builder.ApplyInfixOperator(left, i.Operator, right);

                case PrefixExpression<T> pre:
                    Expression<T> preOperand = pre.Operand.TransformRecursive(transform, builder);
                    return ReferenceEquals(pre.Operand, preOperand)
                        ? pre : builder.ApplyPrefixOperator(pre.Operator, preOperand);

                case PostfixExpression<T> post:
                    Expression<T> postOperand = post.Operand.TransformRecursive(transform, builder);
                    return ReferenceEquals(post.Operand, postOperand)
                        ? post : builder.ApplyPostfixOperator(postOperand, post.Operator);

                default: throw new NotImplementedException(orig.GetType().Name);
            }
        }

        /*
        public static string PrintWrapped<T>(this Expression<T> expr, int lineLength = 80, int indent = 4)
        {
            List<Token> ts = [.. expr.GetTokens()];
            StringBuilder result = new();
            StringBuilder line = new();

            foreach (Token t in ts)
            {
                if (t is OperatorToken ot && ot.Definition.IsInfix)
                {
                    if (line.Length + t.Content.Length + 2 > lineLength) AdvanceLine();
                    line.Append(' ').Append(ot.Content).Append(' ');
                }
                else if (t is StructuralToken { Type: StructuralToken.Types.CloseParenthesis })
                {
                    line.Append(t.Content);
                }
                else
                {
                    if (line.Length + t.Content.Length > lineLength) AdvanceLine();
                    line.Append(t.Content);
                }
            }
            result.AppendLine(line.ToString());
            return result.ToString();

            void AdvanceLine()
            {
                result.AppendLine(line.ToString());
                line.Clear().Append(' ', indent);
            }
        }
        */
    }
}

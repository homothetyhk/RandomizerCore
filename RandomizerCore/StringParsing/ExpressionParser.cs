namespace RandomizerCore.StringParsing
{
    /// <summary>
    /// Parses <see cref="IExpression{T}"/>s from a stream of <see cref="Token"/>s.
    /// </summary>
    public class ExpressionParser<T>
    {
        private readonly IOperatorProvider operatorProvider;
        private readonly IExpressionFactory<T> expressionFactory;
        private readonly IReadOnlyList<Token> tokenStream;

        private int index = 0;

        /// <summary>
        /// Constructs a parser
        /// </summary>
        /// <param name="operatorProvider">The operator definition provider to use while parsing</param>
        /// <param name="expressionFactory">The expression factory to use while parsing</param>
        /// <param name="tokenStream">The token stream to parse from</param>
        public ExpressionParser(IOperatorProvider operatorProvider, IExpressionFactory<T> expressionFactory, IReadOnlyList<Token> tokenStream)
        {
            this.operatorProvider = operatorProvider;
            this.expressionFactory = expressionFactory;
            this.tokenStream = tokenStream;
        }

        /// <summary>
        /// Parses an expression
        /// </summary>
        /// <exception cref="ParsingException">When an unrecoverable parsing error occurs</exception>
        public IExpression<T> Parse()
        {
            IExpression<T> expr = PrattParser(0);
            if (!IsEmpty())
            {
                throw new ParsingException($"Unmatched closing parenthesis ')' at position {tokenStream[index - 1].StartCharacter}.");
            }
            return expr;
        }

        private IExpression<T> PrattParser(int minBindingPower)
        {
            Token next = Next();
            IExpression<T> lhs;
            if (expressionFactory.IsAtom(next))
            {
                lhs = expressionFactory.CreateAtomExpression(next);
            }
            else if (next is OperatorToken ot)
            {
                int? prefixRbp = operatorProvider.PrefixBindingPower(ot.Operator);
                if (prefixRbp != null)
                {
                    IExpression<T> operand = PrattParser(prefixRbp.Value);
                    lhs = expressionFactory.CreatePrefixExpression(ot, operand);
                }
                else
                {
                    throw new ParsingException($"Expected a prefix operator at position {ot.StartCharacter} " +
                        $"but got a different operator '{ot.Operator}'.");
                }
            }
            else if (next is StructuralToken st && st.TokenType == StructuralToken.Type.OpenParenthesis)
            {
                lhs = PrattParser(0);
                Token closingParen = Next();
                if (closingParen is not StructuralToken nst || nst.TokenType != StructuralToken.Type.CloseParenthesis)
                {
                    throw new ParsingException($"Unmatched opening parenthesis '(' at position {st.StartCharacter}.");
                }
                lhs = new GroupingExpression<T>(st, lhs, nst);
            }
            else if (next is StructuralToken st2 && st2.TokenType == StructuralToken.Type.CloseParenthesis)
            {
                throw new ParsingException($"Unmatched closing parenthesis ')' at position {st2.StartCharacter}.");
            }
            else
            {
                throw new ParsingException($"Unexpected token '{next.Print()}' at position {next.StartCharacter}.");
            }

            while (true)
            {
                // it's end-of-input, there's no operator or RHS
                if (IsEmpty())
                {
                    break;
                }

                Token op = Peek();
                if (op is StructuralToken st && st.TokenType == StructuralToken.Type.CloseParenthesis)
                {
                    break;
                }

                if (op is not OperatorToken ot)
                {
                    throw new ParsingException($"Expected an operator at position {op.StartCharacter}");
                }

                // postfix handling is a special case; it should not fail out if the operator is not
                // known because it might be infix.
                if (operatorProvider.PostfixBindingPower(ot.Operator) is int postLbp)
                {
                    if (postLbp < minBindingPower)
                    {
                        // the incoming binding power is stronger than the binding power of the next operator, so we will steal
                        // the operand into our expression and call it a day (that operator will then later pull us into its expression)
                        break;
                    }
                    Next();
                    lhs = expressionFactory.CreatePostfixExpression(lhs, ot);
                    continue;
                }

                (int, int)? ibp = operatorProvider.InfixBindingPower(ot.Operator);
                if (ibp != null)
                {
                    (int lbp, int rbp) = ibp.Value;
                    if (lbp < minBindingPower)
                    {
                        // the incoming binding power is stronger than the binding power of the next operator, so we will steal
                        // the operand into our expression and call it a day (that operator will then later pull us into its expression)
                        break;
                    }
                    Next();
                    IExpression<T> rhs = PrattParser(rbp);

                    lhs = expressionFactory.CreateInfixExpression(lhs, ot, rhs);
                }
                else
                {
                    throw new ParsingException($"Expected an infix operator at position {ot.StartCharacter} " +
                        $"but got a different operator '{ot.Operator}'.");
                }
            }
            return lhs;
        }

        private bool IsEmpty() => index >= tokenStream.Count;
        private Token Peek() => tokenStream[index];
        private Token Next() => tokenStream[index++];
    }
}

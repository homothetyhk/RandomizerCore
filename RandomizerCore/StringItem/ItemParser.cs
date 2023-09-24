using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.StringItem
{
    public class ItemParser
    {
        private int index = 0;
        private IReadOnlyList<Token> tokenStream;
        public ItemParser(IReadOnlyList<Token> tokenStream)
        {
            this.tokenStream = tokenStream;
        }

        public IExpression Parse()
        {
            IExpression expr = PrattParser(0);
            if (!IsEmpty())
            {
                throw new Exception($"Unmatched closing parenthesis ')' at position {tokenStream[index - 1].StartCharacter}.");
            }
            return expr;
        }

        private IExpression PrattParser(int minBindingPower)
        {
            Token next = Next();
            IExpression lhs;
            if (AtomExpression.IsAtomToken(next))
            {
                lhs = new AtomExpression(next);
            }
            else if (next is OperatorToken ot)
            {
                int prefixRbp = Operators.PrefixBindingPower(ot.Operator);
                IExpression operand = PrattParser(prefixRbp);
                lhs = PrefixExpression.ForOperand(ot, operand);
            }
            else if (next is StructuralToken st && st.TokenType == StructuralToken.Type.OpenParenthesis)
            {
                lhs = PrattParser(0);
                Token closingParen = Next();
                if (closingParen is not StructuralToken nst || nst.TokenType != StructuralToken.Type.CloseParenthesis)
                {
                    // todo - better exception
                    throw new Exception($"Unmatched opening parenthesis '(' at position {st.StartCharacter}.");
                }
                lhs = new GroupingExpression(st, lhs, nst);
            }
            else
            {
                // handle prefix ops, parens, error cases appropriately
                throw new NotImplementedException();
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
                    throw new Exception($"Expected an operator at position {op.StartCharacter}"); // todo - better exception
                }

                if (Operators.PostfixBindingPower(ot.Operator) is int postLbp)
                {
                    if (postLbp < minBindingPower)
                    {
                        // the incoming binding power is stronger than the binding power of the next operator, so we will steal
                        // the operand into our expression and call it a day (that operator will then later pull us into its expression)
                        break;
                    }
                    Next();
                    lhs = PostfixExpression.ForOperand(ot, lhs);
                    continue;
                }

                (int lbp, int rbp) = Operators.InfixBindingPower(ot.Operator);
                if (lbp < minBindingPower)
                {
                    // the incoming binding power is stronger than the binding power of the next operator, so we will steal
                    // the operand into our expression and call it a day (that operator will then later pull us into its expression)
                    break;
                }
                Next();
                IExpression rhs = PrattParser(rbp);

                lhs = InfixExpression.ForOperands(ot, lhs, rhs);
            }
            return lhs;
        }

        private bool IsEmpty() => index >= tokenStream.Count;
        private Token Peek() => tokenStream[index];
        private Token Next() => tokenStream[index++];
    }
}

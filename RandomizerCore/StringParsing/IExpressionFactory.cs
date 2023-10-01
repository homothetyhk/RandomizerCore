using System;
using System.Collections.Generic;
using System.Text;

namespace RandomizerCore.StringParsing
{
    public interface IExpressionFactory<T>
    {
        bool IsAtom(Token token);
        IExpression<T> CreateAtomExpression(Token token);
        IExpression<T> CreatePrefixExpression(OperatorToken op, IExpression<T> operand);
        IExpression<T> CreatePostfixExpression(IExpression<T> operand, OperatorToken op);
        IExpression<T> CreateInfixExpression(IExpression<T> left, OperatorToken op, IExpression<T> right);
    }
}

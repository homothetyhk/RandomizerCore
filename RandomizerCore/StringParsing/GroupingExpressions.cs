using System;
using System.Collections.Generic;
using System.Text;

namespace RandomizerCore.StringParsing
{
    public record GroupingExpression<T>(StructuralToken OpenParenthesis, IExpression<T> Nested, StructuralToken CloseParenthesis) : IExpression<T>
    {
        public IEnumerable<T> Evaluate() => Nested.Evaluate();
        public bool Validate() => OpenParenthesis.TokenType == StructuralToken.Type.OpenParenthesis
            && CloseParenthesis.TokenType == StructuralToken.Type.CloseParenthesis
            && Nested.Validate();
        public string Print() => OpenParenthesis.Print() + Nested.Print() + CloseParenthesis.Print();
    }
}

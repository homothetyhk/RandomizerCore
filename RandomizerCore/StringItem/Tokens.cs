using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.StringItem
{
    public abstract record Token
    {
        public int StartCharacter;
        public int EndCharacter;
        public string LeadingTrivia;
        public string TrailingTrivia;

        public string Print() => LeadingTrivia + PrintContent() + TrailingTrivia;
        protected abstract string PrintContent();
    }

    /// <summary>
    /// Token representing a term, logicdef, or item name
    /// </summary>
    public record NameToken : Token
    {
        public string Value;

        protected override string PrintContent() => Value;
    }

    public record NumberToken : Token
    {
        public int Value;

        protected override string PrintContent() => Value.ToString();
    }

    public record LogicStringToken : Token
    {
        public string Value;

        protected override string PrintContent() => Value;
    }

    public record StructuralToken : Token
    {
        public enum Type
        {
            OpenParenthesis,
            CloseParenthesis
        }
        public Type TokenType;

        protected override string PrintContent() => TokenType switch
        {
            Type.OpenParenthesis => "(",
            Type.CloseParenthesis => ")",
            _ => throw new NotImplementedException()
        };
    }

    public record OperatorToken : Token
    {
        public string Operator;

        protected override string PrintContent() => Operator;
    }
}

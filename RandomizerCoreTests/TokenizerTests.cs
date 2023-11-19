using FluentAssertions;
using RandomizerCore.StringItems;
using RandomizerCore.StringParsing;

namespace RandomizerCoreTests
{
    public class TokenizerTests
    {
        [Fact]
        public void TestTokenizeBasic()
        {
            string input = "  Grubsong+=1 >> `Grubsong = 1` => CHARMS += 1";
            Tokenizer tokenizer = new(new ItemOperatorProvider(), input, '`');
            List<Token> tokens = tokenizer.Tokenize();

            tokens.Should().HaveCount(9).And.ContainInConsecutiveOrder(
                new NameToken { Value = "Grubsong", LeadingTrivia = "  ", TrailingTrivia = "", StartCharacter = 2, EndCharacter = 9 },
                new OperatorToken { Operator = "+=", LeadingTrivia = "", TrailingTrivia = "", StartCharacter = 10, EndCharacter = 11 },
                new NumberToken { Value = 1, LeadingTrivia = "", TrailingTrivia = " ", StartCharacter = 12, EndCharacter = 12 },
                new OperatorToken { Operator = ">>", LeadingTrivia = "", TrailingTrivia = " ", StartCharacter = 14, EndCharacter = 15 },
                new StringToken { Value = "Grubsong = 1", LeadingTrivia = "`", TrailingTrivia = "` ", StartCharacter = 18, EndCharacter = 29},
                new OperatorToken { Operator = "=>", LeadingTrivia = "", TrailingTrivia = " ", StartCharacter = 32, EndCharacter = 33 },
                new NameToken { Value = "CHARMS", LeadingTrivia = "", TrailingTrivia = " ", StartCharacter = 35, EndCharacter = 40 },
                new OperatorToken { Operator = "+=", LeadingTrivia = "", TrailingTrivia = " ", StartCharacter = 42, EndCharacter = 43 },
                new NumberToken { Value = 1, LeadingTrivia = "", TrailingTrivia = "", StartCharacter = 45, EndCharacter = 45 }
            );
            string.Join("", tokens.Select(x => x.Print())).Should().Be(input);
        }

        private class TestOperatorProvider : IOperatorProvider
        {
            public IReadOnlyCollection<string> GetAllOperators() => new[] { ">|" };
            public (int, int)? InfixBindingPower(string op) => throw new NotImplementedException();
            public int? PostfixBindingPower(string op) => throw new NotImplementedException();
            public int? PrefixBindingPower(string op) => throw new NotImplementedException();
        }
        /// <summary>
        /// Regression test for a bug where operator tokenization would appear to work correctly when the only characters
        /// in an operator were valid first characters for operators, but would fail to advance the tokenizer tree traversal
        /// correctly and throw a key error otherwise
        /// </summary>
        [Fact]
        public void TestOperatorTokenizationWithUniqueSecondCharacterAdvancesTraversal()
        {
            string input = ">|";
            Tokenizer tokenizer = new(new TestOperatorProvider(), input, null);
            List<Token> tokens = tokenizer.Tokenize();
            tokens.Should().ContainSingle().Which.Should().Be(new OperatorToken
            {
                StartCharacter = 0, EndCharacter = 1,
                LeadingTrivia = "", TrailingTrivia = "",
                Operator = ">|"
            });
        }
    }
}
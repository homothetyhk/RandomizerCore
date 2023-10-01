using FluentAssertions;
using RandomizerCore.StringItem;
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
    }
}
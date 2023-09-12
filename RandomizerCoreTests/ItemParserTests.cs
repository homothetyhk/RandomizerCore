using FluentAssertions;
using RandomizerCore.StringItem;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCoreTests
{
    public class ItemParserTests
    {
        [Fact]
        public void TestParserBasic()
        {
            // string input = "  Grubsong+=1 >> `Grubsong = 1` => CHARMS += 1";
            List<Token> tokens = new()
            {
                new NameToken { Value = "Grubsong", LeadingTrivia = "  ", TrailingTrivia = "", StartCharacter = 2, EndCharacter = 9 },
                new OperatorToken { Operator = "+=", LeadingTrivia = "", TrailingTrivia = "", StartCharacter = 10, EndCharacter = 11 },
                new NumberToken { Value = 1, LeadingTrivia = "", TrailingTrivia = " ", StartCharacter = 12, EndCharacter = 12 },
                new OperatorToken { Operator = ">>", LeadingTrivia = "", TrailingTrivia = " ", StartCharacter = 14, EndCharacter = 15 },
                new LogicStringToken { Value = "Grubsong = 1", LeadingTrivia = "`", TrailingTrivia = "` ", StartCharacter = 18, EndCharacter = 29},
                new OperatorToken { Operator = "=>", LeadingTrivia = "", TrailingTrivia = " ", StartCharacter = 32, EndCharacter = 33 },
                new NameToken { Value = "CHARMS", LeadingTrivia = "", TrailingTrivia = " ", StartCharacter = 35, EndCharacter = 40 },
                new OperatorToken { Operator = "+=", LeadingTrivia = "", TrailingTrivia = " ", StartCharacter = 42, EndCharacter = 43 },
                new NumberToken { Value = 1, LeadingTrivia = "", TrailingTrivia = "", StartCharacter = 45, EndCharacter = 45 }
            };

            ItemParser parser = new(tokens);
            IExpression expr = parser.Parse();

            expr.Should().BeEquivalentTo(new ChainingExpression(
                new AdditionAssignmentExpression(
                    new AtomExpression(tokens[0]),
                    new AtomExpression(tokens[2])
                ),
                new ConditionalExpression(
                    new AtomExpression(tokens[4]),
                    new AdditionAssignmentExpression(
                        new AtomExpression(tokens[6]), 
                        new AtomExpression(tokens[8])
                    )
                )
            ));
            expr.Validate().Should().BeTrue("this is a valid expression");
            expr.Evaluate().Should().BeEquivalentTo(new[] { EvaluatedType.ItemEffect });
        }

        [Fact]
        public void TestParserParens()
        {
            List<Token> tokens = new ItemTokenizer("(((Grubsong)))").Tokenize();
            IExpression expr = new ItemParser(tokens).Parse();
            expr.Should().BeEquivalentTo(new AtomExpression(
                new NameToken
                {
                    StartCharacter = 3, EndCharacter = 10,
                    LeadingTrivia = "", TrailingTrivia = "",
                    Value = "Grubsong"
                }
            ));
        }
    }
}

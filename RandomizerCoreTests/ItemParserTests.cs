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
                    (OperatorToken)tokens[1],
                    new AtomExpression(tokens[2])
                ),
                (OperatorToken)tokens[3],
                new ConditionalExpression(
                    new AtomExpression(tokens[4]),
                    (OperatorToken)tokens[5],
                    new AdditionAssignmentExpression(
                        new AtomExpression(tokens[6]),
                        (OperatorToken)tokens[7],
                        new AtomExpression(tokens[8])
                    )
                )
            ));
            expr.Validate().Should().BeTrue("this is a valid expression");
            expr.Evaluate().Should().BeEquivalentTo(new[] { EvaluatedType.ItemEffect });
            expr.Print().Should().Be("  Grubsong+=1 >> `Grubsong = 1` => CHARMS += 1");
        }

        [Fact]
        public void TestParserPostfix()
        {
            // string input = "  Grubsong++ >> `Grubsong = 1` => CHARMS += 1";
            List<Token> tokens = new()
            {
                new NameToken { Value = "Grubsong", LeadingTrivia = "  ", TrailingTrivia = "", StartCharacter = 2, EndCharacter = 9 },
                new OperatorToken { Operator = "++", LeadingTrivia = "", TrailingTrivia = " ", StartCharacter = 10, EndCharacter = 11 },
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
                new IncrementExpression(
                    new AtomExpression(tokens[0]),
                    (OperatorToken)tokens[1]
                ),
                (OperatorToken)tokens[2],
                new ConditionalExpression(
                    new AtomExpression(tokens[3]),
                    (OperatorToken)tokens[4],
                    new AdditionAssignmentExpression(
                        new AtomExpression(tokens[5]),
                        (OperatorToken)tokens[6],
                        new AtomExpression(tokens[7])
                    )
                )
            ));
            expr.Validate().Should().BeTrue("this is a valid expression");
            expr.Evaluate().Should().BeEquivalentTo(new[] { EvaluatedType.ItemEffect });
            expr.Print().Should().Be("  Grubsong++ >> `Grubsong = 1` => CHARMS += 1");
        }

        [Fact]
        public void TestParserParens()
        {
            List<Token> tokens = new ItemTokenizer("(((Grubsong)))").Tokenize();
            IExpression expr = new ItemParser(tokens).Parse();
            expr.Should().BeEquivalentTo(new GroupingExpression(
                new StructuralToken
                {
                    StartCharacter = 0,
                    EndCharacter = 0,
                    LeadingTrivia = "",
                    TrailingTrivia = "",
                    TokenType = StructuralToken.Type.OpenParenthesis
                },
                new GroupingExpression(
                    new StructuralToken
                    {
                        StartCharacter = 1,
                        EndCharacter = 1,
                        LeadingTrivia = "",
                        TrailingTrivia = "",
                        TokenType = StructuralToken.Type.OpenParenthesis
                    },
                    new GroupingExpression(
                        new StructuralToken
                        {
                            StartCharacter = 2,
                            EndCharacter = 2,
                            LeadingTrivia = "",
                            TrailingTrivia = "",
                            TokenType = StructuralToken.Type.OpenParenthesis
                        },
                        new AtomExpression(
                            new NameToken
                            {
                                StartCharacter = 3,
                                EndCharacter = 10,
                                LeadingTrivia = "",
                                TrailingTrivia = "",
                                Value = "Grubsong"
                            }
                        ),
                        new StructuralToken
                        {
                            StartCharacter = 11,
                            EndCharacter = 11,
                            LeadingTrivia = "",
                            TrailingTrivia = "",
                            TokenType = StructuralToken.Type.CloseParenthesis
                        }
                    ),
                    new StructuralToken
                    {
                        StartCharacter = 12,
                        EndCharacter = 12,
                        LeadingTrivia = "",
                        TrailingTrivia = "",
                        TokenType = StructuralToken.Type.CloseParenthesis
                    }
                ),
                new StructuralToken
                {
                    StartCharacter = 13,
                    EndCharacter = 13,
                    LeadingTrivia = "",
                    TrailingTrivia = "",
                    TokenType = StructuralToken.Type.CloseParenthesis
                }
            ));
            expr.Validate().Should().BeTrue("this is a valid expression, just not a valid item effect");
            expr.Evaluate().Should().BeEquivalentTo(new[] { EvaluatedType.TermLike, EvaluatedType.Bool });
            expr.Print().Should().Be("(((Grubsong)))");
        }
    }
}

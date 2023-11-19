using FluentAssertions;
using RandomizerCore;
using RandomizerCore.Exceptions;
using RandomizerCore.Logic;
using RandomizerCore.StringItems;
using RandomizerCore.StringParsing;

namespace RandomizerCoreTests
{
    public class StringItemEndToEndTests
    {
        [Theory]
        [InlineData(new object[] { "10 += 10 >> 10", new[] {
            "[0:1] Expected an expression of type TermLike but got one of type Int.",
            "[12:13] Expected an expression of type ItemEffect but got one of type Int."
        } })]
        [InlineData(new object[] { "Grubsong += 1 += 2", new[] {
            "[0:12] Expected an expression of type TermLike but got one of type ItemEffect."
        } })]
        public void TestParsableInvalidExpression(string input, string[] errors)
        {
            ItemOperatorProvider operatorProvider = new();
            Tokenizer tokenizer = new(operatorProvider, input, '`');
            IReadOnlyList<Token> tokens = tokenizer.Tokenize();
            ExpressionParser<ItemExpressionType> parser = new(operatorProvider, new ItemExpressionFactory(), tokens);
            IExpression<ItemExpressionType> expr = parser.Parse();
            ExpressionValidator<ItemExpressionType> validator = new();
            expr.Validate(validator).Should().BeFalse();
            validator.Errors.Should().HaveCount(errors.Length).And.Contain(errors);
        }

        [Fact]
        public void TestShadeCloak()
        {
            string input = """
                `LEFTDASH<1 | RIGHTDASH<1 | LEFTDASH<2 + RIGHTDASH<2`
                    => (`LEFTDASH + RIGHTDASH`
                        => (LEFTDASH++ >> RIGHTDASH++)
                        >|> `LEFTDASH<1 + RIGHTDASH>1`
                            => LEFTDASH += 2
                            >|> LEFTDASH++)
                """.Replace("\r", "");
            IExpression<ItemExpressionType> expected = new ConditionalExpression(
                    new ItemAtomExpression(new StringToken
                    {
                        StartCharacter = 1, EndCharacter = 51,
                        LeadingTrivia = "`", TrailingTrivia = "`\n    ",
                        Value = "LEFTDASH<1 | RIGHTDASH<1 | LEFTDASH<2 + RIGHTDASH<2"
                    }),
                    new OperatorToken
                    {
                        StartCharacter = 58, EndCharacter = 59,
                        LeadingTrivia = "", TrailingTrivia = " ",
                        Operator = "=>"
                    },
                    new GroupingExpression<ItemExpressionType>(
                        new StructuralToken
                        {
                            StartCharacter = 61, EndCharacter = 61,
                            LeadingTrivia = "", TrailingTrivia = "",
                            TokenType = StructuralToken.Type.OpenParenthesis
                        },
                        new ShortCircuitChainingExpression(
                            new ShortCircuitChainingExpression(
                                new ConditionalExpression(
                                    new ItemAtomExpression(new StringToken
                                    {
                                        StartCharacter = 63, EndCharacter = 82,
                                        LeadingTrivia = "`", TrailingTrivia = "`\n        ",
                                        Value = "LEFTDASH + RIGHTDASH"
                                    }),
                                    new OperatorToken
                                    {
                                        StartCharacter = 93, EndCharacter = 94,
                                        LeadingTrivia = "", TrailingTrivia = " ",
                                        Operator = "=>"
                                    },
                                    new GroupingExpression<ItemExpressionType>(
                                        new StructuralToken
                                        {
                                            StartCharacter = 96, EndCharacter = 96,
                                            LeadingTrivia = "", TrailingTrivia = "",
                                            TokenType = StructuralToken.Type.OpenParenthesis
                                        },
                                        new ChainingExpression(
                                            new IncrementExpression(
                                                new ItemAtomExpression(new NameToken
                                                {
                                                    StartCharacter = 97, EndCharacter = 104,
                                                    LeadingTrivia = "", TrailingTrivia = "",
                                                    Value = "LEFTDASH"
                                                }),
                                                new OperatorToken
                                                {
                                                    StartCharacter = 105, EndCharacter = 106,
                                                    LeadingTrivia = "", TrailingTrivia = " ",
                                                    Operator = "++"
                                                }
                                            ),
                                            new OperatorToken
                                            {
                                                StartCharacter = 108, EndCharacter = 109,
                                                LeadingTrivia = "", TrailingTrivia = " ",
                                                Operator = ">>"
                                            },
                                            new IncrementExpression(
                                                new ItemAtomExpression(new NameToken
                                                {
                                                    StartCharacter = 111, EndCharacter = 119,
                                                    LeadingTrivia = "", TrailingTrivia = "",
                                                    Value = "RIGHTDASH"
                                                }),
                                                new OperatorToken
                                                {
                                                    StartCharacter = 120, EndCharacter = 121,
                                                    LeadingTrivia = "", TrailingTrivia = "",
                                                    Operator = "++"
                                                }
                                            )
                                        ),
                                        new StructuralToken
                                        {
                                            StartCharacter = 122, EndCharacter = 122,
                                            LeadingTrivia = "", TrailingTrivia = "\n        ",
                                            TokenType = StructuralToken.Type.CloseParenthesis
                                        }
                                    )
                                ),
                                new OperatorToken
                                {
                                    StartCharacter = 132, EndCharacter = 134,
                                    LeadingTrivia = "", TrailingTrivia = " ",
                                    Operator = ">|>"
                                },
                                new ConditionalExpression(
                                    new ItemAtomExpression(new StringToken
                                    {
                                        StartCharacter = 137, EndCharacter = 160,
                                        LeadingTrivia = "`", TrailingTrivia = "`\n            ",
                                        Value = "LEFTDASH<1 + RIGHTDASH>1"
                                    }),
                                    new OperatorToken
                                    {
                                        StartCharacter = 175, EndCharacter = 176,
                                        LeadingTrivia = "", TrailingTrivia = " ",
                                        Operator = "=>"
                                    },
                                    new AdditionAssignmentExpression(
                                        new ItemAtomExpression(new NameToken
                                        {
                                            StartCharacter = 178, EndCharacter = 185,
                                            LeadingTrivia = "", TrailingTrivia = " ",
                                            Value = "LEFTDASH"
                                        }),
                                        new OperatorToken
                                        {
                                            StartCharacter = 187, EndCharacter = 188,
                                            LeadingTrivia = "", TrailingTrivia = " ",
                                            Operator = "+="
                                        },
                                        new ItemAtomExpression(new NumberToken
                                        {
                                            StartCharacter = 190, EndCharacter = 190,
                                            LeadingTrivia = "", TrailingTrivia = "\n            ",
                                            Value = 2
                                        })
                                    )
                                )
                            ),
                            new OperatorToken
                            {
                                StartCharacter = 204, EndCharacter = 206,
                                LeadingTrivia = "", TrailingTrivia = " ",
                                Operator = ">|>"
                            },
                            new IncrementExpression(
                                new ItemAtomExpression(new NameToken
                                {
                                    StartCharacter = 208, EndCharacter = 215,
                                    LeadingTrivia = "", TrailingTrivia = "",
                                    Value = "LEFTDASH"
                                }),
                                new OperatorToken
                                {
                                    StartCharacter = 216, EndCharacter = 217,
                                    LeadingTrivia = "", TrailingTrivia = "",
                                    Operator = "++"
                                }
                            )
                        ),
                        new StructuralToken
                        {
                            StartCharacter = 218, EndCharacter = 218,
                            LeadingTrivia = "", TrailingTrivia = "",
                            TokenType = StructuralToken.Type.CloseParenthesis
                        }
                    )
                );

            ItemOperatorProvider operatorProvider = new();
            Tokenizer tokenizer = new(operatorProvider, input, '`');
            IReadOnlyList<Token> tokens = tokenizer.Tokenize();
            ExpressionParser<ItemExpressionType> parser = new(operatorProvider, new ItemExpressionFactory(), tokens);
            IExpression<ItemExpressionType> expr = parser.Parse();
            ExpressionValidator<ItemExpressionType> validator = new();
            expr.Validate(validator).Should().BeTrue();
            validator.Errors.Should().BeEmpty();

            expr.Should().BeEquivalentTo(expected);
        }
    }
}

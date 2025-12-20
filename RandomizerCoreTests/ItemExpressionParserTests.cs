using FluentAssertions;
using RandomizerCore.StringItems;
using RandomizerCore.StringParsing;

namespace RandomizerCoreTests
{
    public class ItemExpressionParserTests
    {
        [Fact]
        public void TestParserBasic()
        {
            // string input = "  Grubsong+=1 >> `Grubsong = 1` => CHARMS += 1";
            ItemOperatorProvider operatorProvider = new();
            List<Token> tokens =
            [
                new NameToken("Grubsong"),
                new OperatorToken(operatorProvider.GetDefinition("+=")!),
                new NumberToken (1),
                new OperatorToken(operatorProvider.GetDefinition(">>")!),
                new StringToken('`', "Grubsong = 1"),
                new OperatorToken(operatorProvider.GetDefinition("=>")!),
                new NameToken("CHARMS"),
                new OperatorToken(operatorProvider.GetDefinition("+=")!),
                new NumberToken(1)
            ];

            ExpressionParser<ItemExpressionType> parser = new(operatorProvider, new ItemExpressionFactory(), tokens);
            Expression<ItemExpressionType> expr = parser.Parse();

            expr.Should().BeEquivalentTo(new ChainingExpression(
                new AdditionAssignmentExpression(
                    new ItemAtomExpression(tokens[0]),
                    (OperatorToken)tokens[1],
                    new ItemAtomExpression(tokens[2])
                ),
                (OperatorToken)tokens[3],
                new ConditionalExpression(
                    new ItemAtomExpression(tokens[4]),
                    (OperatorToken)tokens[5],
                    new AdditionAssignmentExpression(
                        new ItemAtomExpression(tokens[6]),
                        (OperatorToken)tokens[7],
                        new ItemAtomExpression(tokens[8])
                    )
                )
            ));
            expr.Validate(out _).Should().BeTrue("this is a valid expression");
            expr.SpeculateType().Should().BeEquivalentTo([ItemExpressionType.ItemEffect]);
            expr.Print().Should().Be("Grubsong += 1 >> `Grubsong = 1` => CHARMS += 1");
        }

        [Fact]
        public void TestParserPostfix()
        {
            // string input = "  Grubsong++ >> `Grubsong = 1` => CHARMS += 1";
            ItemOperatorProvider operatorProvider = new();
            List<Token> tokens =
            [
                new NameToken("Grubsong"!),
                new OperatorToken(operatorProvider.GetDefinition("++")!),
                new OperatorToken(operatorProvider.GetDefinition(">>")!),
                new StringToken('`', "Grubsong = 1"),
                new OperatorToken(operatorProvider.GetDefinition("=>")!),
                new NameToken("CHARMS"),
                new OperatorToken(operatorProvider.GetDefinition("+=")!),
                new NumberToken(1)
            ];

            ExpressionParser<ItemExpressionType> parser = new(operatorProvider, new ItemExpressionFactory(), tokens);
            Expression<ItemExpressionType> expr = parser.Parse();

            expr.Should().BeEquivalentTo(new ChainingExpression(
                new IncrementExpression(
                    new ItemAtomExpression(tokens[0]),
                    (OperatorToken)tokens[1]
                ),
                (OperatorToken)tokens[2],
                new ConditionalExpression(
                    new ItemAtomExpression(tokens[3]),
                    (OperatorToken)tokens[4],
                    new AdditionAssignmentExpression(
                        new ItemAtomExpression(tokens[5]),
                        (OperatorToken)tokens[6],
                        new ItemAtomExpression(tokens[7])
                    )
                )
            ));
            expr.Validate(out _).Should().BeTrue("this is a valid expression");
            expr.SpeculateType().Should().BeEquivalentTo([ItemExpressionType.ItemEffect]);
            expr.Print().Should().Be("Grubsong++ >> `Grubsong = 1` => CHARMS += 1");
        }

        [Fact]
        public void TestParserAssociativity()
        {
            // string input = "`Grubsong = 0` => `Grubsong < 1` => CHARMS++ >> Grubsong++";
            ItemOperatorProvider operatorProvider = new();
            List<Token> tokens =
            [
                new StringToken('`', "Grubsong = 0"),
                new OperatorToken(operatorProvider.GetDefinition("=>")!),
                new StringToken('`', "Grubsong < 1"),
                new OperatorToken(operatorProvider.GetDefinition("=>")!),
                new NameToken("CHARMS"),
                new OperatorToken(operatorProvider.GetDefinition("++")!),
                new OperatorToken(operatorProvider.GetDefinition(">>")!),
                new NameToken("Grubsong"),
                new OperatorToken(operatorProvider.GetDefinition("++")!) 
            ];

            ExpressionParser<ItemExpressionType> parser = new(new ItemOperatorProvider(), new ItemExpressionFactory(), tokens);
            Expression<ItemExpressionType> expr = parser.Parse();
            expr.Should().BeEquivalentTo(new ChainingExpression(
                new ConditionalExpression(
                    new ItemAtomExpression(tokens[0]),
                    (OperatorToken)tokens[1],
                    new ConditionalExpression(
                        new ItemAtomExpression(tokens[2]),
                        (OperatorToken)tokens[3],
                        new IncrementExpression(
                            new ItemAtomExpression(tokens[4]),
                            (OperatorToken)tokens[5]
                        )
                    )
                ),
                (OperatorToken)tokens[6],
                new IncrementExpression(
                    new ItemAtomExpression(tokens[7]),
                    (OperatorToken)tokens[8]
                )
            ));
            expr.Validate(out _).Should().BeTrue("this is a valid expression");
            expr.SpeculateType().Should().BeEquivalentTo([ItemExpressionType.ItemEffect]);
            expr.Print().Should().Be("`Grubsong = 0` => `Grubsong < 1` => CHARMS++ >> Grubsong++");
        }

        [Fact]
        public void TestParserParens()
        {
            // string input = "(((Grubsong)))";
            List<Token> tokens =
            [
                new StructuralToken(StructuralToken.Types.OpenParenthesis),
                new StructuralToken(StructuralToken.Types.OpenParenthesis),
                new StructuralToken(StructuralToken.Types.OpenParenthesis),
                new NameToken("Grubsong"),
                new StructuralToken(StructuralToken.Types.CloseParenthesis),
                new StructuralToken(StructuralToken.Types.CloseParenthesis),
                new StructuralToken(StructuralToken.Types.CloseParenthesis),
            ];
            ExpressionParser<ItemExpressionType> parser = new(new ItemOperatorProvider(), new ItemExpressionFactory(), tokens);
            Expression<ItemExpressionType> expr = parser.Parse();
            expr.Should().BeEquivalentTo(new GroupingExpression<ItemExpressionType>(
                (StructuralToken)tokens[0],
                new GroupingExpression<ItemExpressionType>(
                    (StructuralToken)tokens[1],
                    new GroupingExpression<ItemExpressionType>(
                        (StructuralToken)tokens[2],
                        new ItemAtomExpression(tokens[3]),
                        (StructuralToken)tokens[4]
                    ),
                    (StructuralToken)tokens[5]
                ),
                (StructuralToken)tokens[6]
            ));
            expr.Validate(out _).Should().BeTrue("this is a valid expression, just not a valid item effect");
            expr.SpeculateType().Should().BeEquivalentTo([ItemExpressionType.TermLike, ItemExpressionType.Bool]);
            expr.Print().Should().Be("(((Grubsong)))");
        }
    }
}

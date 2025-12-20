using FluentAssertions;
using RandomizerCore.StringLogic;
using RandomizerCore.StringParsing;
using OperatorToken = RandomizerCore.StringParsing.OperatorToken;

namespace RandomizerCoreTests
{
    public class LogicExpressionParserTests
    {
        [Fact]
        public void TestTokenizerBasic()
        {
            string input = "A ? B + * C / | D > E";
            LogicOperatorProvider operatorProvider = new();
            List<Token> result = Tokenizer.Tokenize(input, operatorProvider, null);

            List<Token> expected = 
            [
                new NameToken("A"),
                new OperatorToken(operatorProvider.GetDefinition("?")!),
                new NameToken("B"),
                new OperatorToken(operatorProvider.GetDefinition("+")!),
                new OperatorToken(operatorProvider.GetDefinition("*")!),
                new NameToken("C"),
                new OperatorToken(operatorProvider.GetDefinition("/")!),
                new OperatorToken(operatorProvider.GetDefinition("|")!),
                new NameToken("D"),
                new OperatorToken(operatorProvider.GetDefinition(">")!),
                new NameToken("E"),
            ];

            result.Should().BeEquivalentTo(expected);
        }

        [Fact]
        public void TestParserBasic()
        {
            // string input = "A ? B + * C / | D > E"; // ((A ? B) + ((* C) /)) | (D > E)
            LogicOperatorProvider operatorProvider = new();
            List<Token> tokenizedInput =
            [
                new NameToken("A"),
                new OperatorToken(operatorProvider.GetDefinition("?")!),
                new NameToken("B"),
                new OperatorToken(operatorProvider.GetDefinition("+")!),
                new OperatorToken(operatorProvider.GetDefinition("*")!),
                new NameToken("C"),
                new OperatorToken(operatorProvider.GetDefinition("/")!),
                new OperatorToken(operatorProvider.GetDefinition("|")!),
                new NameToken("D"),
                new OperatorToken(operatorProvider.GetDefinition(">")!),
                new NameToken("E"),
            ];
            LogicExpressionFactory lef = new();
            ExpressionParser<LogicExpressionType> parser = new(operatorProvider, lef, tokenizedInput);
            Expression<LogicExpressionType> expr = parser.Parse();

            Expression<LogicExpressionType> expected =
                new OrExpression(
                    new AndExpression(
                        new CoalesceExpression(
                            new LogicAtomExpression(tokenizedInput[0]),
                            (OperatorToken)tokenizedInput[1],
                            new LogicAtomExpression(tokenizedInput[2])
                        ),
                        (OperatorToken)tokenizedInput[3],
                        new ProjectionExpression(
                            new ReferenceExpression(
                                (OperatorToken)tokenizedInput[4],
                                new LogicAtomExpression(tokenizedInput[5])
                            ),
                            (OperatorToken)tokenizedInput[6]
                        )
                    ),
                    (OperatorToken)tokenizedInput[7],
                    new ComparisonExpression(
                        new LogicAtomExpression(tokenizedInput[8]),
                        (OperatorToken)tokenizedInput[9],
                        new LogicAtomExpression(tokenizedInput[10])
                    ));

            expr.Should().BeEquivalentTo(expected);
            expr.Print().Should().Be("A ? B + *C/ | D>E");
            expr.Validate(out _).Should().BeTrue();
        }

        [Fact]
        public void NewSyntaxTest()
        {
            LogicClause c = new("(A ? B) > 1");
            LogicOperatorProvider op = new();
            LogicExpressionBuilder builder = new();
            Expression<LogicExpressionType> expected = builder.ApplyInfixOperator(
                builder.ApplyInfixOperator(
                    builder.NameAtom("A"),
                    builder.Op("?"),
                    builder.NameAtom("B")),
                builder.Op(">"),
                builder.NumberAtom(1));
            c.Expr.Should().BeEquivalentTo(expected);

            c = new("*(A ? B)");
            expected = builder.ApplyPrefixOperator(
                builder.Op("*"),
                builder.ApplyInfixOperator(
                    builder.NameAtom("A"),
                    builder.Op("?"),
                    builder.NameAtom("B")));
            c.Expr.Should().BeEquivalentTo(expected);

            c = new("(A + B | C)/");
            expected = builder.ApplyPostfixOperator(
                builder.ApplyInfixOperator(
                    builder.ApplyInfixOperator(
                        builder.NameAtom("A"),
                        builder.Op("+"),
                        builder.NameAtom("B")),
                    builder.Op("|"),
                    builder.NameAtom("C")),
                builder.Op("/"));
            c.Expr.Should().BeEquivalentTo(expected);
        }


    }
}

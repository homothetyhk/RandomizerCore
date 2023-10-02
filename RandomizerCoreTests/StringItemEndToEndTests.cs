using FluentAssertions;
using RandomizerCore.StringItem;
using RandomizerCore.StringParsing;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
    }
}

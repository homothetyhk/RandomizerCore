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
            ItemOperatorProvider operatorProvider = new();
            List<Token> tokens = Tokenizer.Tokenize(input, operatorProvider, '`');

            tokens.Should().HaveCount(9).And.ContainInConsecutiveOrder(
                new NameToken("Grubsong"),
                new OperatorToken(operatorProvider.GetDefinition("+=")!),
                new NumberToken(1),
                new OperatorToken(operatorProvider.GetDefinition(">>")!),
                new StringToken('`', "Grubsong = 1"),
                new OperatorToken(operatorProvider.GetDefinition("=>")!),
                new NameToken("CHARMS"),
                new OperatorToken(operatorProvider.GetDefinition("+=")!),
                new NumberToken(1)
            );
            string.Join("", tokens.Select(x => x.Print())).Should().Be("Grubsong+=1>>`Grubsong = 1`=>CHARMS+=1");
        }

        private class TestOperatorProvider : IOperatorProvider
        {
            private readonly Dictionary<string, OperatorDefinition> operatorDefinitions = new()
            {
                [">|"] = new(">|", null, null),
            };

            public OperatorDefinition? GetDefinition(string op) => operatorDefinitions.TryGetValue(op, out OperatorDefinition? def) ? def : null;
            public IReadOnlyCollection<string> GetAllOperators() => operatorDefinitions.Keys;
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
            TestOperatorProvider operatorProvider = new();
            List<Token> tokens = Tokenizer.Tokenize(input, operatorProvider, '`');
            tokens.Should().ContainSingle().Which.Should().Be(new OperatorToken(operatorProvider.GetDefinition(">|")!));
        }
    }
}
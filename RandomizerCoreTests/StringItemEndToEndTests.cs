using FluentAssertions;
using RandomizerCore;
using RandomizerCore.Exceptions;
using RandomizerCore.Logic;
using RandomizerCore.StringItems;
using RandomizerCore.StringParsing;
using Xunit.Abstractions;

namespace RandomizerCoreTests
{
    public class StringItemEndToEndTests
    {
        public ITestOutputHelper Output { get; }
        public StringItemEndToEndTests(ITestOutputHelper Output)
        {
            this.Output = Output;
        }


        [Theory]
        [InlineData(["10 += 10 >> 10", new[] {
            "[0:1] Expected an expression of type TermLike but got one of type Int.",
            "[4:5] Expected an expression of type ItemEffect but got one of type Int."
        } ])]
        [InlineData(["Grubsong += 1 += 2", new[] {
            "[0:3] Expected an expression of type TermLike but got one of type ItemEffect."
        } ])]
        public void TestParsableInvalidExpression(string input, string[] errors)
        {
            ItemOperatorProvider operatorProvider = new();
            IReadOnlyList<Token> tokens = Tokenizer.Tokenize(input, operatorProvider, '`');
            ExpressionParser<ItemExpressionType> parser = new(operatorProvider, new ItemExpressionFactory(), tokens);
            Expression<ItemExpressionType> expr = parser.Parse();
            expr.Validate(out ExpressionValidator<ItemExpressionType> validator).Should().BeFalse();
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
            ItemOperatorProvider operatorProvider = new();

            Expression<ItemExpressionType> expected = new ConditionalExpression(
                    new ItemAtomExpression(new StringToken('`', "LEFTDASH<1 | RIGHTDASH<1 | LEFTDASH<2 + RIGHTDASH<2")),
                    new OperatorToken(operatorProvider.GetDefinition("=>")!),
                    new GroupingExpression<ItemExpressionType>(
                        new StructuralToken(StructuralToken.Types.OpenParenthesis),
                        new ShortCircuitChainingExpression(
                            new ShortCircuitChainingExpression(
                                new ConditionalExpression(
                                    new ItemAtomExpression(new StringToken('`', "LEFTDASH + RIGHTDASH")),
                                    new OperatorToken(operatorProvider.GetDefinition("=>")!),
                                    new GroupingExpression<ItemExpressionType>(new StructuralToken(StructuralToken.Types.OpenParenthesis),
                                        new ChainingExpression(
                                            new IncrementExpression(
                                                new ItemAtomExpression(new NameToken("LEFTDASH")),
                                                new OperatorToken(operatorProvider.GetDefinition("++")!)
                                            ),
                                            new OperatorToken(operatorProvider.GetDefinition(">>")!),
                                            new IncrementExpression(
                                                new ItemAtomExpression(new NameToken("RIGHTDASH")),
                                                new OperatorToken(operatorProvider.GetDefinition("++")!)
                                            )
                                        ),
                                        new StructuralToken(StructuralToken.Types.CloseParenthesis)
                                    )
                                ),
                                new OperatorToken(operatorProvider.GetDefinition(">|>")!),
                                new ConditionalExpression(
                                    new ItemAtomExpression(new StringToken('`', "LEFTDASH<1 + RIGHTDASH>1")),
                                    new OperatorToken(operatorProvider.GetDefinition("=>")!),
                                    new AdditionAssignmentExpression(
                                        new ItemAtomExpression(new NameToken("LEFTDASH")),
                                        new OperatorToken(operatorProvider.GetDefinition("+=")!),
                                        new ItemAtomExpression(new NumberToken(2))
                                    )
                                )
                            ),
                            new OperatorToken(operatorProvider.GetDefinition(">|>")!),
                            new IncrementExpression(
                                new ItemAtomExpression(new NameToken("LEFTDASH")),
                                new OperatorToken(operatorProvider.GetDefinition("++")!)
                            )
                        ),
                        new StructuralToken(StructuralToken.Types.CloseParenthesis)
                    )
                );

            
            IReadOnlyList<Token> tokens = Tokenizer.Tokenize(input, operatorProvider, '`');
            ExpressionParser<ItemExpressionType> parser = new(operatorProvider, new ItemExpressionFactory(), tokens);
            Expression<ItemExpressionType> expr = parser.Parse();
            expr.Validate(out ExpressionValidator<ItemExpressionType> validator).Should().BeTrue();
            validator.Errors.Should().BeEmpty();

            expr.Should().BeEquivalentTo(expected);
        }

        [Theory]
        [InlineData(new string[] { "A", }, "A++", new int[] { 1 })]
        [InlineData(new string[] { "A", }, "A    +=   2    >|>     A     ++", new int[] { 2 })]
        [InlineData(new string[] { "A", "B" }, "`B=1` => A++ >|> B++ >> `B=1` => A++ >|> B++", new int[] { 1, 1 })]
        [InlineData(new string[] { "A", "B" }, "C?++ >|> (B++ >> B++) >> ((B++ >> B++) >> (B++ >> C?+=2)) >> !`A=1` => A++ >|> B++", new int[] { 1, 5 })]
        [InlineData(new string[] { "A", "B" }, "C?++ >|> (B++ >|> B++) >> ((B++ >|> B++) >|> (B++ >|> C?+=2)) >> !`A=1` => A++ >|> B++", new int[] { 1, 2 })]
        public void TestLMParser(string[] terms, string itemString, int[] termValues)
        {
            LogicManagerBuilder lmb = new();
            Term[] ts = terms.Select(lmb.GetOrAddTerm).ToArray();
            LogicManager lm = new(lmb);
            LogicItem li = lm.FromItemString("test_item", itemString);
            ProgressionManager pm = new(lm, null);
            pm.Add(li);
            ts.Select(t => pm.Get(t)).Should().Equal(termValues);
        }

        [Theory]
        [InlineData("A++", "*Item_A >|> A += 2", 1)]
        [InlineData("C?++", "*Item_A >|> A += 2", 2)]
        public void TestItemReference(string itemStringA, string itemStringB, int termValue)
        {
            LogicManagerBuilder lmb = new();
            Term a = lmb.GetOrAddTerm("A");
            lmb.AddItem(new StringItemTemplate("Item_A", itemStringA));
            lmb.AddItem(new StringItemTemplate("Item_B", itemStringB));
            LogicManager lm = new(lmb);
            LogicItem itemB = lm.GetItemStrict("Item_B");
            ProgressionManager pm = new(lm, null);
            pm.Add(itemB);
            pm.Get(a).Should().Be(termValue);
        }

        [Fact]
        public void TestItemCycleError()
        {
            LogicManagerBuilder lmb = new();
            lmb.AddItem(new StringItemTemplate("Item_A", "*Item_B"));
            lmb.AddItem(new StringItemTemplate("Item_B", "*Item_C"));
            lmb.AddItem(new StringItemTemplate("Item_C", "*Item_A"));
            lmb.Invoking(lmb => new LogicManager(lmb)).Should().Throw<ReferenceCycleException>();
        }

        [Fact]
        public void TestNestedConditional()
        {
            LogicManagerBuilder lmb = new();
            Term a = lmb.GetOrAddTerm("A");
            LogicManager lm = new(lmb);
            ProgressionManager pm = new(lm, null);
            LogicItem item = lm.FromItemString("I", "`A>3` => `A<5` => (A++ >> A += 2 >> A += 3)");
            ConditionalEffect e = ((StringItem)item).Effect.Should().BeOfType<ConditionalEffect>().Subject;
            e.Logic.InfixSource.Should().Be("A>3");
            e = e.Effect.Should().BeOfType<ConditionalEffect>().Subject;
            e.Logic.InfixSource.Should().Be("A<5");
            e.Effect.Should().Be(new AllOfEffect([new IncrementEffect(1, a), new IncrementEffect(2, a), new IncrementEffect(3, a)]));

            pm.Set(a, 3);
            pm.Add(item);
            pm.Get(a).Should().Be(3);
            pm.Set(a, 5);
            pm.Add(item);
            pm.Get(a).Should().Be(5);
            pm.Set(a, 4);
            pm.Add(item);
            pm.Get(a).Should().Be(10);
        }
    }
}

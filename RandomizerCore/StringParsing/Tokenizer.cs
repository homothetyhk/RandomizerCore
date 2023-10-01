using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RandomizerCore.StringParsing
{
    public class Tokenizer
    {
        private enum TokenizingState
        {
            LeadingTrivia,
            Content,
            TrailingTrivia
        }

        private readonly IOperatorProvider operatorProvider;
        private readonly HashSet<char> reservedOperatorChars;
        private readonly OperatorTokenizerTree operatorTokenizerTree;
        private readonly string input;
        private readonly char? stringDelimiter;

        private int cursor = 0;
        private TokenizingState state;
        private int startingIndex;
        private int endingIndex;
        private StringBuilder leadingTrivia;
        private StringBuilder content;
        private StringBuilder trailingTrivia;

        public Tokenizer(IOperatorProvider operatorProvider, string input, char? stringDelimiter = null)
        {
            this.operatorProvider = operatorProvider;
            this.reservedOperatorChars = new HashSet<char>();
            this.operatorTokenizerTree = new OperatorTokenizerTree();
            foreach (string op in operatorProvider.GetAllOperators())
            {
                this.reservedOperatorChars.UnionWith(op);
                this.operatorTokenizerTree.Insert(op);
            }

            this.input = input;
            this.stringDelimiter = stringDelimiter;
            StartToken();
        }

        public List<Token> Tokenize()
        {
            List<Token> tokens = new();
            while (!IsEmpty())
            {
                // we start each token by looking for leading trivia (usually whitespace)
                if (state != TokenizingState.LeadingTrivia)
                {
                    throw new InvalidOperationException($"Tokenizing in bad state - expected " +
                        $"LeadingTrivia, was {state} at position {cursor}. " +
                        $"Current partial token: {leadingTrivia}{content}{trailingTrivia}");
                }
                while (char.IsWhiteSpace(Peek()))
                {
                    Consume();
                }

                Token tokenInProgress;
                char next = Peek();
                if (next == '(')
                {
                    AdvanceState();
                    Consume();
                    tokenInProgress = new StructuralToken
                    {
                        TokenType = StructuralToken.Type.OpenParenthesis
                    };
                    AdvanceState();
                }
                else if (next == ')')
                {
                    AdvanceState();
                    Consume();
                    tokenInProgress = new StructuralToken
                    {
                        TokenType = StructuralToken.Type.CloseParenthesis
                    };
                    AdvanceState();
                }
                else if (stringDelimiter != null && next == stringDelimiter)
                {
                    tokenInProgress = ReadStringToken();
                }
                else if (reservedOperatorChars.Contains(next))
                {
                    tokenInProgress = ReadOperatorToken();
                }
                else
                {
                    tokenInProgress = ReadNameOrNumericToken();
                }

                // and just as we started, we end by eating excess whitespace
                if (state != TokenizingState.TrailingTrivia)
                {
                    throw new InvalidOperationException($"Tokenizing in bad state - expected " +
                        $"TrailingTrivia, was {state} at position {cursor}. " +
                        $"Current partial token: {leadingTrivia}{content}{trailingTrivia}");
                }
                while (!IsEmpty() && char.IsWhiteSpace(Peek()))
                {
                    Consume();
                }

                tokenInProgress.StartCharacter = startingIndex;
                tokenInProgress.EndCharacter = endingIndex;
                tokenInProgress.LeadingTrivia = leadingTrivia.ToString();
                tokenInProgress.TrailingTrivia = trailingTrivia.ToString();
                tokens.Add(tokenInProgress);
                StartToken();
            }
            return tokens;
        }

        // precondition: stringDelimiter must be nonnull
        private Token ReadStringToken()
        {
            Expect(stringDelimiter.Value);
            AdvanceState();
            while (Peek() != stringDelimiter.Value)
            {
                Consume();
                if (IsEmpty())
                {
                    // todo - better exception
                    throw new Exception($"Encountered unterminated logic string starting at position {startingIndex}");
                }
            }
            AdvanceState();
            Expect(stringDelimiter.Value);
            return new StringToken
            {
                Value = content.ToString(),
            };
        }

        private Token ReadOperatorToken()
        {
            AdvanceState();
            char next;
            OperatorTokenizerTree tree = operatorTokenizerTree;
            // todo - this approach misses cases where operators are not leaf nodes. For example, if we added
            // an operator >|, and the sequence >|* appeared, that could be tokenized as >| and *, but would instead
            // fail because >|> exists and so there is a length-3 candidate, and * is not >, so the Expect will explode.
            // making this generic and not requiring backtracking might be hard so maybe it shouldn't be supported?
            while (reservedOperatorChars.Contains(next = Peek()) && tree.Candidates.Count > 0)
            {
                Expect(tree.Candidates.Contains);
                tree = operatorTokenizerTree.Advance(next);
                if (IsEmpty())
                {
                    // we hit EOF mid-parse; we have to forcibly terminate, but make sure we got a real operator
                    if (!operatorProvider.GetAllOperators().Contains(tree.Value))
                    {
                        // todo - better exception
                        throw new Exception($"Invalid operator `{tree.Value}` at position {startingIndex}");
                    }
                    break;
                }
            }
            AdvanceState();
            return new OperatorToken
            {
                Operator = content.ToString()
            };
        }

        private Token ReadNameOrNumericToken()
        {
            AdvanceState();
            Expect(IsValidNameOrNumberCharacter);
            while (!IsEmpty() && IsValidNameOrNumberCharacter(Peek()))
            {
                Consume();
            }
            AdvanceState();

            string value = content.ToString();
            if (int.TryParse(value, out int intValue))
            {
                return new NumberToken { Value = intValue };
            }
            else
            {
                return new NameToken { Value = value };
            }
        }

        private bool IsEmpty() => cursor >= input.Length;
        private bool IsValidNameOrNumberCharacter(char ch) =>
            !"()`".Contains(ch)
            && !reservedOperatorChars.Contains(ch)
            && !char.IsWhiteSpace(ch);

        private char Peek() => input[cursor];
        private void Consume()
        {
            StringBuilder currentBuilder = state switch
            {
                TokenizingState.LeadingTrivia => leadingTrivia,
                TokenizingState.Content => content,
                TokenizingState.TrailingTrivia => trailingTrivia,
                _ => throw new NotImplementedException()
            };
            currentBuilder.Append(input[cursor++]);
            if (state == TokenizingState.LeadingTrivia)
            {
                startingIndex++;
            }
        }

        private void Expect(char ch) => Expect(next => ch == next);
        private void Expect(Predicate<char> predicate)
        {
            char next = Peek();
            if (!predicate(next))
            {
                // todo - better exception
                throw new Exception($"Bad character {next} at position {cursor}.");
            }
            Consume();
        }

        private void AdvanceState()
        {
            if (state == TokenizingState.LeadingTrivia)
            {
                state = TokenizingState.Content;
            }
            else if (state == TokenizingState.Content)
            {
                state = TokenizingState.TrailingTrivia;
                endingIndex = cursor - 1;
            }
        }

        private void StartToken()
        {
            startingIndex = cursor;
            state = TokenizingState.LeadingTrivia;
            leadingTrivia = new StringBuilder();
            content = new StringBuilder();
            trailingTrivia = new StringBuilder();
        }
    }
}

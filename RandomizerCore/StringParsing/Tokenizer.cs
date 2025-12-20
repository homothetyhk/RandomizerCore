namespace RandomizerCore.StringParsing
{
    /// <summary>
    /// Converts a raw input string to a <see cref="Token"/> stream.
    /// </summary>
    public static class Tokenizer
    {
        /// <summary>
        /// Tokenizes the input.
        /// </summary>
        /// <param name="input">The input string.</param>
        /// <param name="operatorProvider">The operator definition provider to use when tokenizing.</param>
        /// <param name="stringDelimiter">
        /// If the grammar supports strings, the delimiting character that appears around strings otherwise null.
        /// </param>
        /// <exception cref="TokenizingException">When an unrecoverable tokenization error occurs</exception>
        public static List<Token> Tokenize(string input, IOperatorProvider operatorProvider, char? stringDelimiter = null)
        {
            List<Token> tokens = [];
            int cursor = 0;

            HashSet<char> reservedOperatorChars = [];
            OperatorTokenizerTree rootTree = new();
            foreach (string op in operatorProvider.GetAllOperators())
            {
                reservedOperatorChars.UnionWith(op);
                rootTree.Insert(op);
            }

            while (cursor < input.Length)
            {
                // remove leading whitespace
                while (char.IsWhiteSpace(input[cursor]))
                {
                    cursor++;
                    if (cursor >= input.Length) return tokens;
                }

                char next = input[cursor];

                if (next == '(')
                {
                    tokens.Add(new StructuralToken(StructuralToken.Types.OpenParenthesis));
                    cursor++;
                    continue;
                }
                else if (next == ')')
                {
                    tokens.Add(new StructuralToken(StructuralToken.Types.CloseParenthesis));
                    cursor++;
                    continue;
                }
                else if (stringDelimiter != null && next == stringDelimiter)
                {
                    cursor++;
                    int startingIndex = cursor;
                    while (input[cursor] != stringDelimiter.Value)
                    {
                        cursor++;
                        if (cursor == input.Length) throw new TokenizingException($"Encountered unterminated string token starting at position {startingIndex}");
                    }
                    tokens.Add(new StringToken(stringDelimiter.Value, input[startingIndex..cursor]));
                    cursor++;
                    continue;
                }
                else if (reservedOperatorChars.Contains(next))
                {
                    int startingIndex = cursor;
                    OperatorTokenizerTree tree = rootTree;

                    while (reservedOperatorChars.Contains(next = input[cursor]) && tree.Candidates.Contains(next))
                    {
                        cursor++;
                        tree = tree.Advance(next);
                        if (cursor >= input.Length)
                        {
                            // we hit EOF mid-parse; we have to forcibly terminate, but make sure we got a real operator
                            if (!operatorProvider.GetAllOperators().Contains(tree.Value))
                            {
                                throw new TokenizingException($"Invalid operator `{tree.Value}` at position {startingIndex}.");
                            }
                            break;
                        }
                    }
                    // there are conditions in which we might branch to an intermediate tree branch that is not a complete operator
                    // for example, consider the case where {ABCD} is the set of reserved characters, A is an operator,
                    // ABC is an operator, AB is not an operator, and the input ABD appears. We can traverse the tree down to AB,
                    // but when D appears, the loop will break and we should fail here because AB is not an operator.
                    if (operatorProvider.GetDefinition(tree.Value) is not OperatorDefinition operatorDefinition)
                    {
                        throw new TokenizingException($"Invalid operator `{tree.Value}` at position {startingIndex}.");
                    }

                    tokens.Add(new OperatorToken(operatorDefinition));
                    continue;
                }
                else
                {
                    int startingIndex = cursor;
                    while (cursor < input.Length)
                    {
                        next = input[cursor];

                        if (stringDelimiter.HasValue && next == stringDelimiter.Value) break;
                        if (next == '(' || next == ')') break;
                        if (reservedOperatorChars.Contains(next)) break;

                        cursor++;
                    }
                        
                    // remove trailing whitespace
                    int endingIndexInc = cursor - 1;
                    while (endingIndexInc > startingIndex && char.IsWhiteSpace(input[endingIndexInc])) endingIndexInc--;
                    string name = input[startingIndex..(endingIndexInc + 1)];
                    if (int.TryParse(name, out int num)) tokens.Add(new NumberToken(num));
                    else tokens.Add(new NameToken(name));

                    continue;
                }
            }
            return tokens;
        }
    }
}

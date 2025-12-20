using RandomizerCore.Extensions;

namespace RandomizerCore.StringLogic
{
    [Obsolete]
    public static class Infix
    {
        private static readonly LogicProcessor _sharedTokenSource = new();

        public static List<LogicToken> Tokenize(string infix)
        {
            return Tokenize(infix, _sharedTokenSource);
        }

        public static List<LogicToken> Tokenize(string infix, ITokenSource tokenSource)
        {
            int i = 0;
            Stack<string> operatorStack = new();
            List<string> postfix = new();

            while (i < infix.Length)
            {
                string op = GetNextOperator(infix, ref i);

                // Easiest way to deal with whitespace between operators
                if (op.Trim() == string.Empty)
                {
                    continue;
                }

                if (ComparerStrings.Contains(op))
                {
                    postfix.Insert(postfix.Count - 1, op);
                    postfix.Add(GetNextOperator(infix, ref i));
                }
                else if (Precedence.TryGetValue(op, out int prec))
                {
                    while (operatorStack.Count != 0 && operatorStack.Peek() != "(" && Precedence[operatorStack.Peek()] >= prec)
                    {
                        postfix.Add(operatorStack.Pop());
                    }

                    operatorStack.Push(op);
                }
                else if (op == "(")
                {
                    operatorStack.Push(op);
                }
                else if (op == ")")
                {
                    while (operatorStack.Peek() != "(")
                    {
                        postfix.Add(operatorStack.Pop());
                    }

                    operatorStack.Pop();
                }
                else
                {
                    postfix.Add(op);
                }
            }

            while (operatorStack.Count != 0)
            {
                postfix.Add(operatorStack.Pop());
            }

            List<LogicToken> output = new();
            for (int j = 0; j < postfix.Count; j++)
            {
                switch (postfix[j])
                {
                    case "|":
                        output.Add(OperatorToken.OR);
                        break;
                    case "+":
                        output.Add(OperatorToken.AND);
                        break;
                    case "?":
                        {
                            TermToken right = (TermToken)output.Pop();
                            TermToken left = (TermToken)output.Pop();
                            output.Add(new CoalescingToken(left, right));
                        }
                        break;
                    case ">":
                    case "<":
                    case "=":
                        {
                            string op = postfix[j];
                            string left = postfix[++j];
                            string right = postfix[++j];
                            output.Add(tokenSource.GetComparisonToken(ComparerEnum[op], left, right));
                        }
                        break;
                    case "(":
                    case ")":
                        throw new ArgumentException($"Failed to tokenize infix {infix}: extra parens found after parsing.");
                    default:
                        {
                            output.Add(tokenSource.GetTermToken(postfix[j]));
                        }
                        break;
                }
            }

            return output;
        }

        public static string ToInfix(IReadOnlyList<LogicToken> tokens)
        {
            Stack<(string text, OperatorToken? outermost)> phrases = new();
            // a "phrase" is a string of logic in infix form
            // the outermost operator is used to determine whether the infix must be parenthesized

            for (int i = 0; i < tokens.Count; i++)
            {
                LogicToken t = tokens[i];
                if (t is OperatorToken op)
                {
                    (string right, OperatorToken? rightOp) = phrases.Pop();
                    (string left, OperatorToken? leftOp) = phrases.Pop();
                    if (rightOp != null && op.Precedence > rightOp.Precedence)
                    {
                        right = $"({right})";
                    }
                    if (leftOp != null && op.Precedence > leftOp.Precedence)
                    {
                        left = $"({left})";
                    }

                    phrases.Push(($"{left} {op.Symbol} {right}", op));
                }
                else if (t is TermToken tt)
                {
                    phrases.Push((tt.Write(), null));
                }
                else if (t is null) throw new NullReferenceException("Null token");
                else throw new ArgumentException($"Unknown token {t}");
            }

            if (phrases.Count != 1) throw new ArgumentException("Malformatted token list--found extra tokens in the stack at the end of parsing.");

            return phrases.Pop().text;
        }

        private static string GetNextOperator(string infix, ref int i)
        {
            int start = i;

            if (SpecialCharacters.Contains(infix[i]))
            {
                i++;
                return infix[i - 1].ToString();
            }

            while (i < infix.Length && !SpecialCharacters.Contains(infix[i]))
            {
                i++;
            }

            return infix[start..i].Trim();
        }

        private static readonly char[] SpecialCharacters = new char[]
        {
            '(', ')', '+', '|', '>', '<', '=', '?'
        };

        // combinators that take terms to a term
        private static readonly string[] ComparerStrings = new string[]
        {
            ">", "<", "="
        };

        private static readonly Dictionary<string, ComparisonType> ComparerEnum = new()
        {
            [">"] = ComparisonType.GT,
            ["<"] = ComparisonType.LT,
            ["="] = ComparisonType.EQ,
        };

        private static readonly Dictionary<string, OperatorType> OperatorEnum = new()
        {
            ["|"] = OperatorType.OR,
            ["+"] = OperatorType.AND
        };

        // +, | are binary infix, bool -> bool -> bool
        private static readonly Dictionary<string, int> Precedence = new()
        {
            { "|", 0 },
            { "+", 1 },
            { "?", 10 },
        };
    }
}

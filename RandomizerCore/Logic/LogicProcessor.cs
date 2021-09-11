using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;

namespace RandomizerCore.Logic
{
    public interface ILogicProcessor
    {
        IList<string> Shunt(string infix);
    }

    public class LogicProcessor : ILogicProcessor
    {
        readonly Dictionary<string, string[]> macros = new Dictionary<string, string[]>();

        public LogicProcessor(Dictionary<string, string> macros)
        {
            foreach (var kvp in macros)
            {
                this.macros[kvp.Key] = Shunt(kvp.Value).ToArray();
            }
        }

        public IList<string> Shunt(string infix)
        {
            int i = 0;
            Stack<string> operatorStack = new Stack<string>();
            List<string> postfix = new List<string>();

            while (i < infix.Length)
            {
                string op = GetNextOperator(infix, ref i);

                // Easiest way to deal with whitespace between operators
                if (op.Trim() == string.Empty)
                {
                    continue;
                }

                if (LeftOpStrings.Contains(op))
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
                    if (macros.TryGetValue(op, out string[] macro))
                    {
                        postfix.AddRange(macro);
                    }
                    else
                    {
                        postfix.Add(op);
                    }
                }
            }

            while (operatorStack.Count != 0)
            {
                postfix.Add(operatorStack.Pop());
            }

            return postfix;
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

            return infix.Substring(start, i - start).Trim();
        }

        public static char[] SpecialCharacters = new char[]
        {
            '(', ')', '+', '|', '>', '<', '='
        };

        // combinators that take terms to a term
        public static string[] LeftOpStrings = new string[]
        {
            ">", "<", "="
        };

        // +, | are binary infix, bool -> bool -> bool
        public static Dictionary<string, int> Precedence = new Dictionary<string, int>
        {
            { "|", 0 },
            { "+", 1 },
        };
    }
}

using RandomizerCore.StringParsing;

namespace RandomizerCore.StringLogic
{
    public static class LogicExpressionUtil
    {
        internal static LogicOperatorProvider OperatorProvider { get; } = new();
        internal static LogicExpressionFactory Factory { get; } = new();
        internal static LogicExpressionBuilder Builder { get; } = new();

        public static Expression<LogicExpressionType> Parse(string input)
        {
            return new ExpressionParser<LogicExpressionType>(
                OperatorProvider,
                Factory,
                Tokenizer.Tokenize(input, OperatorProvider, null)
            ).Parse();
        }
    }

    public class LogicExpressionBuilder : ExpressionBuilder<LogicExpressionType>
    {
        public LogicExpressionBuilder() : base(LogicExpressionUtil.OperatorProvider, LogicExpressionUtil.Factory)
        {
        }
    }
}

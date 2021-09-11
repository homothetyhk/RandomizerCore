using System;
using System.Diagnostics;

namespace RandomizerCore
{
    public static class LogHelper
    {
        public static event Action<string> OnLog;

        public static void Log(string message) => OnLog?.Invoke(message);

        public static void Log() => OnLog?.Invoke(string.Empty);

        public static void Log<T>(T t) where T : struct => OnLog?.Invoke(t.ToString());

        public static void Log(object o) => OnLog?.Invoke(o.ToString());

        [Conditional("DEBUG")] // compiler remove these calls in release mode
        public static void LogDebug(string message) => OnLog?.Invoke(message);

    }
}

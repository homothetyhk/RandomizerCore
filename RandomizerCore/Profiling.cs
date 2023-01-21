using System.Diagnostics;

namespace RandomizerCore
{
    public static class Profiling
    {
        private static readonly Dictionary<string, List<double>> revertedMetrics = new();
        private static readonly Dictionary<string, List<double>> committedMetrics = new();
        private static readonly Dictionary<string, List<double>> metrics = new();

        /// <summary>
        /// Fully resets all committed, uncommitted, and reverted datapoints.
        /// </summary>
        [Conditional("DEBUG")]
        public static void Reset()
        {
            metrics.Clear();
            revertedMetrics.Clear();
            committedMetrics.Clear();
        }

        /// <summary>
        /// Emits a datapoint for the specified metric
        /// </summary>
        /// <param name="name">The metric name. Should be unique to the code being profiled.</param>
        /// <param name="value">The value to emit.</param>
        [Conditional("DEBUG")]
        internal static void EmitMetric(string name, double value)
        {
            if (!metrics.ContainsKey(name))
            {
                metrics[name] = new();
            }
            metrics[name].Add(value);
        }

        /// <summary>
        /// Commits all staged emitted metrics.
        /// </summary>
        [Conditional("DEBUG")]
        public static void Commit()
        {
            foreach (var metric in metrics)
            {
                if (committedMetrics.ContainsKey(metric.Key))
                {
                    committedMetrics[metric.Key].AddRange(metric.Value);
                }
                else
                {
                    committedMetrics[metric.Key] = metric.Value;
                }
            }
            metrics.Clear();
        }

        /// <summary>
        /// Reverts all staged emitted metrics.
        /// </summary>
        [Conditional("DEBUG")]
        public static void Revert()
        {
            foreach (var metric in metrics)
            {
                if (revertedMetrics.ContainsKey(metric.Key))
                {
                    revertedMetrics[metric.Key].AddRange(metric.Value);
                }
                else
                {
                    revertedMetrics[metric.Key] = metric.Value;
                }
            }
            metrics.Clear();
        }

        /// <summary>
        /// Aggregates and logs metrics in a tabular format. Staged (uncommitted) metrics are not included.
        /// </summary>
        /// <param name="includeReverted">Whether to include reverted metrics in the aggregation.</param>
        [Conditional("DEBUG")]
        public static void Log(bool includeReverted = false)
        {
            const string rowSeparator = "+----------------------------------------------------+----------------+----------------+-----------+";
            LogDebug(rowSeparator);
            LogDebug("| Name                                               |           Mean |        Std Dev |         N |");
            LogDebug(rowSeparator);

            IEnumerable<string> metricsToLog = committedMetrics.Keys;
            if (includeReverted)
            {
                metricsToLog = metricsToLog.Concat(revertedMetrics.Keys);
            }

            foreach (string name in metricsToLog.Distinct())
            {
                List<double> values = new();
                if (committedMetrics.TryGetValue(name, out List<double> committedValues))
                {
                    values.AddRange(committedValues);
                }
                if (includeReverted && revertedMetrics.TryGetValue(name, out List<double> revertedValues))
                {
                    values.AddRange(revertedValues);
                }
                double sum = values.Sum();
                double n = values.Count;
                double mean = sum / n;
                // use sample standard deviation - we're sampling out of an approximately infinitely large population
                double squaredSampleVariance = values.Select(x => (x - mean) * (x - mean)).Sum() / (n - 1);
                double sampleStdDev = Math.Sqrt(squaredSampleVariance);
                LogDebug($"| {name,-50} | {mean,14:g7} | {sampleStdDev,14:g7} | {n,9} |");
                LogDebug(rowSeparator);
            }
        }
    }
}

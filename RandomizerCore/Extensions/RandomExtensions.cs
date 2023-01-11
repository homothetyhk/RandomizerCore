using System.Diagnostics.CodeAnalysis;

namespace RandomizerCore.Extensions
{
    public static class RandomExtensions
    {
        /// <summary>
        /// Removes and returns the last element of the list.
        /// </summary>
        public static T Pop<T>(this IList<T> list)
        {
            T val = list[^1];
            list.RemoveAt(list.Count - 1);
            return val;
        }

        /// <summary>
        /// Removes and returns the element of the list at the requested index.
        /// </summary>
        public static T Pop<T>(this IList<T> list, int index = 0)
        {
            T val = list[index];
            list.RemoveAt(index);
            return val;
        }

        /// <summary>
        /// Removes and returns the first element of the list which satisfies the predicate.
        /// </summary>
        public static T Pop<T>(this IList<T> list, Predicate<T> TSelector)
        {
            return list.Pop(Enumerable.Range(0, list.Count).First(j => TSelector(list[j])));
        }

        /// <summary>
        /// Returns the segment of the list starting at the specified index, with the specified count.
        /// </summary>
        public static IEnumerable<T> Slice<T>(this IReadOnlyList<T> list, int start, int count)
        {
            for (int i = start; i < start + count; i++) yield return list[i];
        }

        /// <summary>
        /// Returns the segment of the array starting at the specified index, with the specified count.
        /// </summary>
        public static IEnumerable<T> Slice<T>(this T[] list, int start, int count)
        {
            for (int i = start; i < start + count; i++) yield return list[i];
        }

        /// <summary>
        /// Searches for the first element of the list which satisfies the predicate. If found, returns true, and removes and outputs that element. Otherwise, returns false.
        /// </summary>
        public static bool TryPop<T>(this IList<T> list, Predicate<T> TSelector, [MaybeNullWhen(false)] out T val)
        {
            int i = Enumerable.Range(0, list.Count).Where(j => TSelector(list[j])).DefaultIfEmpty(-1).First();
            if (i < 0)
            {
                val = default;
                return false;
            }
            else
            {
                val = list.Pop(i);
                return true;
            }
        }

        /// <summary>
        /// Selects an element of the list uniformly at random.
        /// </summary>
        public static T Next<T>(this Random rand, IReadOnlyList<T> ts)
        {
            if (ts.Count == 0) throw new ArgumentOutOfRangeException(nameof(ts));
            return ts[rand.Next(ts.Count)];
        }

        /// <summary>
        /// Selects an element of the list randomly according to the provided weights.
        /// <br/>If cumulative, weights should be an increasing sequence of positive numbers with last entry 1. Probability of selecting element i for i>0 is weights[i] - weights[i-1], and weights[0] for i=0.
        /// <br/>If noncumulative, weights should be a sequence of nonnegative numbers. Probability of selecting element i is weights[i] / total, where total is the sum of the weights.
        /// </summary>
        public static T Next<T>(this Random rand, IReadOnlyList<T> ts, IEnumerable<double> weights, bool cumulativeWeights = false)
        {
            if (ts.Count == 0) throw new ArgumentOutOfRangeException(nameof(ts));
            double d = rand.NextDouble();
            int i = GetIndexFromWeights(d, weights, cumulativeWeights);
            if (i > ts.Count) throw new ArgumentOutOfRangeException(nameof(ts));
            return ts[i];
        }

        /// <summary>
        /// Removes and returns an element of the list, selected uniformly at random.
        /// </summary>
        public static T PopNext<T>(this Random rand, IList<T> ts)
        {
            if (ts.Count == 0) throw new ArgumentOutOfRangeException(nameof(ts));
            return ts.Pop(rand.Next(ts.Count));
        }

        /// <summary>
        /// Removes and returns an element of the list, selected randomly according to the provided weights.
        /// <br/>If cumulative, weights should be an increasing sequence of positive numbers with last entry 1. Probability of selecting element i for i>0 is weights[i] - weights[i-1], and weights[0] for i=0.
        /// <br/>If noncumulative, weights should be a sequence of nonnegative numbers. Probability of selecting element i is weights[i] / total, where total is the sum of the weights.
        /// </summary>
        public static T PopNext<T>(this Random rand, IList<T> ts, IEnumerable<double> weights, bool cumulativeWeights = false)
        {
            if (ts.Count == 0) throw new ArgumentOutOfRangeException(nameof(ts));
            double d = rand.NextDouble();
            int i = GetIndexFromWeights(d, weights, cumulativeWeights);
            if (i > ts.Count) throw new ArgumentOutOfRangeException(nameof(ts));
            return ts.Pop(i);
        }

        private static int GetIndexFromWeights(double d, IEnumerable<double> weights, bool cumulativeWeights)
        {
            int i = 0;
            if (cumulativeWeights)
            {
                foreach (double weight in weights)
                {
                    if (d > weight) break;
                    ++i;
                }
            }
            else
            {
                double total = weights.Sum();
                double cdf = 0.0;
                foreach (double weight in weights)
                {
                    cdf += weight / total;
                    if (d > cdf) break;
                    ++i;
                }
            }
            return i;
        }


        /// <summary>
        /// Returns a new array containing a random permutation of the integers from 0 to n-1.
        /// </summary>
        public static int[] Permute(this Random rand, int n)
        {
            int[] arr = new int[n];
            for (int i = 0; i < n; i++) arr[i] = i;
            rand.PermuteInPlace(arr);
            return arr;
        }

        /// <summary>
        /// Returns a new array containing a random permutation of the elements of the input.
        /// </summary>
        public static T[] Permute<T>(this Random rand, T[] input)
        {
            T[] output = (T[])input.Clone();
            rand.PermuteInPlace(output);
            return output;
        }

        /// <summary>
        /// Randomly permutes the input array.
        /// </summary>
        public static void PermuteInPlace<T>(this Random rand, T[] input)
        {
            for (int i = input.Length - 1; i > 0; i--)
            {
                int j = rand.Next(i + 1);
                T temp = input[i];
                input[i] = input[j];
                input[j] = temp;
            }
        }

        /// <summary>
        /// Returns a list containing a random permutation of the output of the enumerable.
        /// </summary>
        public static List<T> Permute<T>(this Random rand, IEnumerable<T> input)
        {
            List<T> output = new(input);
            rand.PermuteInPlace(output);
            return output;
        }

        /// <summary>
        /// Randomly permutes the input list.
        /// </summary>
        public static void PermuteInPlace<T>(this Random rand, IList<T> input)
        {
            for (int i = input.Count - 1; i > 0; i--)
            {
                int j = rand.Next(i + 1);
                T temp = input[i];
                input[i] = input[j];
                input[j] = temp;
            }
        }

        /// <summary>
        /// Randomly permutes the input list, and performs an action on each element and its new index.
        /// </summary>
        public static void PermuteInPlace<T>(this Random rand, IList<T> input, Action<T, int> action)
        {
            for (int i = input.Count - 1; i > 0; i--)
            {
                int j = rand.Next(i + 1);
                T temp = input[i];
                input[i] = input[j];
                input[j] = temp;
            }

            for (int i = 0; i < input.Count; i++) action.Invoke(input[i], i);
        }

        /// <summary>
        /// Swaps array elements at two indices.
        /// </summary>
        public static void Swap<T>(this T[] arr, int i, int j)
        {
            (arr[j], arr[i]) = (arr[i], arr[j]);
        }

        /// <summary>
        /// Returns a random bool.
        /// </summary>
        public static bool NextBool(this Random rng)
        {
            return rng.Next(2) == 0;
        }

        /// <summary>
        /// Returns a random double within the requested interval, according to a power distribution.
        /// <br/>Equivalent to scaling the interval by the reciprocal power, selecting a double within that interval uniformly, and rescaling the result by the power.
        /// </summary>
        public static double PowerLaw(this Random rng, double pow, double min, double max)
        {
            double scaledMin = Math.Pow(min, 1 / pow);
            double scaledMax = Math.Pow(max, 1 / pow);
            double t = scaledMin + (scaledMax - scaledMin) * rng.NextDouble();
            return Math.Pow(t, pow);
        }

        /// <summary>
        /// Rounds toward 0 to the largest integer multiple of the divisor which is less than the double in magnitude.
        /// </summary>
        public static int ClampToMultipleOf(this double d, int divisor)
        {
            int value = (int)d;
            value -= value % divisor;
            return value;
        }

        /// <summary>
        /// Creates a list of elements which satisfy the predicate, and returns one of the elements of the list uniformly at random.
        /// </summary>
        public static T NextWhere<T>(this Random rng, IReadOnlyList<T> ts, Predicate<T> test)
        {
            List<int> matches = new();
            for (int i = 0; i < ts.Count; i++)
            {
                if (test(ts[i])) matches.Add(i);
            }
            return ts[rng.Next(matches)];
        }
    }
}

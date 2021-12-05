using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RandomizerCore.Extensions
{
    public static class RandomExtensions
    {
        public static T Pop<T>(this IList<T> list)
        {
            T val = list[^1];
            list.RemoveAt(list.Count - 1);
            return val;
        }

        public static T Pop<T>(this IList<T> list, int index = 0)
        {
            T val = list[index];
            list.RemoveAt(index);
            return val;
        }

        public static T Pop<T>(this IList<T> list, Predicate<T> TSelector)
        {
            return list.Pop(Enumerable.Range(0, list.Count).First(j => TSelector(list[j])));
        }

        public static IEnumerable<T> Slice<T>(this IReadOnlyList<T> list, int start, int count)
        {
            for (int i = start; i < start + count; i++) yield return list[i];
        }

        public static IEnumerable<T> Slice<T>(this T[] list, int start, int count)
        {
            for (int i = start; i < start + count; i++) yield return list[i];
        }

        public static bool TryPop<T>(this IList<T> list, Predicate<T> TSelector, out T val)
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

        public static T Next<T>(this Random rand, IReadOnlyList<T> ts)
        {
            return ts[rand.Next(ts.Count())];
        }

        public static T PopNext<T>(this Random rand, IList<T> ts)
        {
            return ts.Pop(rand.Next(ts.Count));
        }

        public static int[] Permute(this Random rand, int n)
        {
            int[] arr = new int[n];
            for (int i = 0; i < n; i++) arr[i] = i;
            rand.PermuteInPlace(arr);
            return arr;
        }

        public static T[] Permute<T>(this Random rand, T[] input)
        {
            T[] output = input.Clone() as T[];
            rand.PermuteInPlace(output);
            return output;
        }

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

        public static List<T> Permute<T>(this Random rand, IEnumerable<T> input)
        {
            List<T> output = new(input);
            rand.PermuteInPlace(output);
            return output;
        }

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

        public static void Swap<T>(this T[] arr, int i, int j)
        {
            T t = arr[i];
            arr[i] = arr[j];
            arr[j] = t;
        }

        public static bool NextBool(this Random rng)
        {
            return rng.Next(2) == 0;
        }

        public static double PowerLaw(this Random rng, double pow, double min, double max)
        {
            double scaledMin = Math.Pow(min, 1 / pow);
            double scaledMax = Math.Pow(max, 1 / pow);
            double t = scaledMin + (scaledMax - scaledMin) * rng.NextDouble();
            return Math.Pow(t, pow);
        }

        public static int ClampToMultipleOf(this double d, int divisor)
        {
            int value = (int)d;
            value -= value % divisor;
            return value;
        }

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

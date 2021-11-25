using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RandomizerCore.Logic;
using RandomizerCore.Randomizers;
using static RandomizerCore.LogHelper;

namespace RandomizerCore
{
    public class TransitionInitializer
    {
        public readonly bool matched;
        public readonly bool coupled;
        public readonly int top;
        public readonly int bot;
        public readonly int left;
        public readonly int right;
        public readonly int oneWayIn;
        public readonly int oneWayOut;
        public readonly int[] totalCounts;
        private readonly (string, int)[] logHelper;
        public readonly int Length;

        public TransitionInitializer(ITransitionRandomizerSettings gs, LogicManager lm)
        {
            matched = gs.Matched;
            coupled = gs.Coupled;
            if (matched)
            {
                top = 0;
                bot = 1;
                left = 2;
                right = 3;
                oneWayIn = 4;
                oneWayOut = 5;
                Length = 6;
                totalCounts = new int[Length];
                logHelper = new[]
                {
                    ("Top", top),
                    ("Bot", bot),
                    ("Left", left),
                    ("Right", right),
                    ("One Way In", oneWayIn),
                    ("One Way Out", oneWayOut),
                };
            }
            else
            {
                top = bot = left = right = 0;
                oneWayIn = 1;
                oneWayOut = 2;
                Length = 3;
                totalCounts = new int[Length];
                logHelper = new[]
                {
                    ("Two Way", right),
                    ("One Way In", oneWayIn),
                    ("One Way Out", oneWayOut),
                };
            }
        }

        public void Initialize(IEnumerable<RandoTransition> ts)
        {
            LogDebug("Initializing transitions...");
            foreach (var t in ts)
            {
                SetTransitionDirection(t);
            }

            LogDebug("First pass complete...");
            LogDebug(this.ToString());

            if (matched)
            {
                foreach (var t in ts.Where(t => t.lt.data.GateName[0] == 'd'))
                {
                    SetDoorDirection(t);
                }
            }

            foreach (var t in ts)
            {
                t.coupled = coupled && t.dir != oneWayIn && t.dir != oneWayOut;
            }

            LogDebug("Second pass complete...");
            LogDebug(this.ToString());
            LogDebug("Transition initialization completed.");
        }

        public string GetDirectionName(int i)
        {
            return logHelper.First(p => p.Item2 == i).Item1;
        }

        public void Reset(IEnumerable<RandoTransition> ts)
        {
            foreach (var t in ts)
            {
                t.placed = State.None;
                t.reachable = State.None;
            }
        }

        private void SetTransitionDirection(RandoTransition t)
        {
            if (t.lt.data.OneWayType == OneWayType.OneWayIn)
            {
                t.dir = oneWayIn;
                t.targetDir = oneWayOut;
                totalCounts[oneWayIn]++;
            }
            else if (t.lt.data.OneWayType == OneWayType.OneWayOut)
            {
                t.dir = oneWayOut;
                t.targetDir = oneWayIn;
                totalCounts[oneWayOut]++;
            }
            else if (!matched)
            {
                t.dir = t.targetDir = right;
                totalCounts[right]++;
            }
            else
            {
                switch (t.lt.data.GateName[0])
                {
                    case 't':
                        t.dir = top;
                        t.targetDir = bot;
                        totalCounts[top]++;
                        break;
                    case 'b':
                        t.dir = bot;
                        t.targetDir = top;
                        totalCounts[bot]++;
                        break;
                    case 'l':
                        t.dir = left;
                        t.targetDir = right;
                        totalCounts[left]++;
                        break;
                    case 'r':
                        t.dir = right;
                        t.targetDir = left;
                        totalCounts[right]++;
                        break;
                    case 'd':
                        return;
                }
            }
        }

        public void SetDoorDirection(RandoTransition t)
        {
            if (totalCounts[left] < totalCounts[right])
            {
                t.dir = left;
                t.targetDir = right;
                totalCounts[left]++;
            }
            else
            {
                t.dir = right;
                t.targetDir = left;
                totalCounts[right]++;
            }
        }

        public override string ToString()
        {
            StringBuilder sb = new();

            foreach (var (name, index) in logHelper)
            {
                sb.AppendLine($"{name}: {totalCounts[index]}");
            }

            return sb.ToString();
        }

        public string PrintCompare(int[] compareTo)
        {
            StringBuilder sb = new();

            int padTo = logHelper.Max(p => p.Item1.Length);
            sb.AppendLine(new string(' ', padTo) + "Total   Value");
            foreach (var (name, index) in logHelper)
            {
                sb.AppendLine(name.PadLeft(padTo) + $": {totalCounts[index]}".PadRight(8) + compareTo[index]);
            }

            return sb.ToString();
        }

    }

}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Randomization
{
    public static class PriorityTransformUtil
    {
        public enum TransformType
        {
            Linear,
            Quadratic,
            SquareRoot,
            Logarithmic
        }

        public enum ItemPriorityDepthEffect
        {
            DefaultCutoff,
            Fade,
            Ignore,
        }

        public static DefaultGroupPlacementStrategy.DepthPriorityTransformHandler CreateTransform
            (float coefficient, TransformType type = TransformType.Linear, ItemPriorityDepthEffect priorityDepthEffect = ItemPriorityDepthEffect.DefaultCutoff)
        {
            coefficient /= 100f;
            return DPT;

            void DPT(IRandoItem item, IRandoLocation location, int itemDepth, int itemPriorityDepth, int locationDepth, ref float locationPriority)
            {
                switch (priorityDepthEffect)
                {
                    case ItemPriorityDepthEffect.DefaultCutoff:
                        if (itemPriorityDepth < locationDepth) return;
                        break;
                    case ItemPriorityDepthEffect.Fade:
                        locationDepth -= itemPriorityDepth;
                        break;
                    default:
                    case ItemPriorityDepthEffect.Ignore:
                        break;
                }
                switch (type)
                {
                    case TransformType.Linear:
                        LinearTransform(coefficient, locationDepth, ref locationPriority);
                        break;
                    case TransformType.Quadratic:
                        QuadraticTransform(coefficient, locationDepth, ref locationPriority);
                        break;
                    case TransformType.SquareRoot:
                        SquareRootTransform(coefficient, locationDepth, ref locationPriority);
                        break;
                    case TransformType.Logarithmic:
                        LogarithmicTransform(coefficient, locationDepth, ref locationPriority);
                        break;
                }
            }
        }

        private static void LinearTransform(float coefficient, int locationDepth, ref float locationPriority)
        {
            locationPriority -= coefficient * locationDepth;
        }

        private static void QuadraticTransform(float coefficient, int locationDepth, ref float locationPriority)
        {
            locationPriority -= coefficient * locationDepth * locationDepth;
        }

        private static void SquareRootTransform(float coefficient, int locationDepth, ref float locationPriority)
        {
            if (locationDepth < 0) return;
            locationPriority -= (float)(coefficient * Math.Sqrt(locationDepth));
        }

        private static void LogarithmicTransform(float coefficient, int locationDepth, ref float locationPriority)
        {
            if (locationDepth <= 0) return;
            locationPriority -= (float)(coefficient * Math.Log(locationDepth));
        }
    }
}

namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Utility class for creating Depth Priority Transforms that do simple mathematical operations.
    /// </summary>
    public static class PriorityTransformUtil
    {
        /// <summary>
        /// Parameter which determines the function applied to the location depth.
        /// </summary>
        public enum TransformType
        {
            Linear,
            Quadratic,
            SquareRoot,
            Logarithmic
        }

        /// <summary>
        /// Parameter which determines how location depth should be adjusted according to item priority depth.
        /// </summary>
        public enum ItemPriorityDepthEffect
        {
            /// <summary>
            /// Cancel priority transform if item priority depth exceeds location depth.
            /// </summary>
            Cliff,
            /// <summary>
            /// Adjust location depth to fade linearly to 0 when greater than item priority depth.
            /// </summary>
            Fade,
            /// <summary>
            /// Clamp location depth to item priority depth as an upper bound.
            /// </summary>
            Cap,
            /// <summary>
            /// Item priority depth has no effect.
            /// </summary>
            Ignore,
        }

        public static DefaultGroupPlacementStrategy.DepthPriorityTransformHandler CreateTransform
            (float coefficient, TransformType type = TransformType.Linear, ItemPriorityDepthEffect priorityDepthEffect = ItemPriorityDepthEffect.Cliff)
        {
            coefficient /= 100f;
            return DPT;

            void DPT(IRandoItem item, IRandoLocation location, int itemDepth, int itemPriorityDepth, int locationDepth, ref float locationPriority)
            {
                switch (priorityDepthEffect)
                {
                    case ItemPriorityDepthEffect.Cliff:
                        if (itemPriorityDepth < locationDepth)
                        {
                            locationPriority += 1f;
                            return;
                        }
                        break;
                    case ItemPriorityDepthEffect.Fade:
                        if (itemPriorityDepth < locationDepth)
                        {
                            locationDepth = Math.Max(2 * itemPriorityDepth - locationDepth, 0);
                        }
                        break;
                    case ItemPriorityDepthEffect.Cap:
                        locationDepth = Math.Min(locationDepth, itemPriorityDepth);
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

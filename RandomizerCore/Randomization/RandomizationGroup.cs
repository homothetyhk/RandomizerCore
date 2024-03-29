﻿namespace RandomizerCore.Randomization
{
    /// <summary>
    /// A group of Items and Locations matched in length, for the randomizer to output in RandoPlacement pairs.
    /// </summary>
    public class RandomizationGroup
    {
        public IRandoItem[] Items;
        public IRandoLocation[] Locations;
        public string Label;
        public GroupPlacementStrategy Strategy;
        public Validator Validator = new();

        /// <summary>
        /// An event invoked after the items and locations of the group are permuted and have their priorities set.
        /// <br/>It is expected that the subscriber may modify the priorities of entries, and the arrays are resorted after the event is invoked.
        /// </summary>
        public event Action<Random, RandomizationGroup>? OnPermute;
        public void InvokeOnPermute(Random rng)
        {
            OnPermute?.Invoke(rng, this);
        }

        /// <summary>
        /// Called on each new attempt of the randomizer.
        /// </summary>
        public virtual void OnNewAttempt() { }

    }
}

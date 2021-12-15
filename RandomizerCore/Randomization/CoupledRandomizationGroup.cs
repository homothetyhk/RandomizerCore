namespace RandomizerCore.Randomization
{
    /// <summary>
    /// Class which represents a RandomizationGroup of coupled elements. All items and locations of a CoupledRandomizationGroup must implement IRandoCouple.
    /// <br/>It is expected that the Items of this group coincide with the Locations of Dual, and vice versa. Both groups must be in the same RandomizationGroup. A group may be self-dual.
    /// <br/>The effect of coupling is that during randomization, Items of the group may be discarded and used as Locations of the dual group, and vice versa. For each placement, the symmetric placement will be generated.
    /// </summary>
    /// <example>
    /// A group of left transition Locations and right transition Items. 
    /// Its dual is the group of right transition Locations and left transition Items, and coupling ensures the transitions will be paired in a reversible manner.
    /// </example>
    public class CoupledRandomizationGroup : RandomizationGroup
    {
        public CoupledRandomizationGroup Dual;
    }
}

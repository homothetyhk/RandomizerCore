using RandomizerCore.Logic;

namespace RandomizerCore
{
    public enum OneWayType
    {
        TwoWay,
        OneWayIn,
        OneWayOut
    }

    public class RandoTransition : IRandoItem, IRandoLocation, IRandoCouple
    {
        public readonly LogicTransition lt;

        public RandoTransition(LogicTransition lt)
        {
            this.lt = lt;
        }

        public float sourcePriority;
        public float targetPriority;

        float IRandoItem.Priority { get => targetPriority; set => targetPriority = value; }
        float IRandoLocation.Priority { get => sourcePriority; set => sourcePriority = value; }

        public int sourceSphere;
        public int targetSphere;
        public bool targetRequired;

        int IRandoItem.Sphere { get => targetSphere; set => targetSphere = value; }
        bool IRandoItem.Required { get => targetRequired; set => targetRequired = value; }
        int IRandoLocation.Sphere { get => sourceSphere; set => sourceSphere = value; }
        

        public State Placed { get; set; }

        public string Name => lt.Name;

        public State Reachable { get; set; }

        public bool CanGet(ProgressionManager pm)
        {
            return lt.CanGet(pm);
        }

        public IEnumerable<Term> GetTerms()
        {
            return lt.GetTerms();
        }

        public void AddTo(ProgressionManager pm)
        {
            pm.Set(lt.term.Id, 1);
        }

        public IEnumerable<Term> GetAffectedTerms()
        {
            return lt.GetAffectedTerms();
        }

        int IComparable<IRandoLocation>.CompareTo(IRandoLocation other)
        {
            return sourcePriority.CompareTo(other.Priority);
        }

        int IComparable<IRandoItem>.CompareTo(IRandoItem other)
        {
            return targetPriority.CompareTo(other.Priority);
        }
    }

}

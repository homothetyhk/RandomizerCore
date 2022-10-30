using RandomizerCore.Logic;
using RandomizerCore.Updater;

namespace RandomizerCore
{
    public enum OneWayType
    {
        TwoWay,
        OneWayIn,
        OneWayOut
    }

    public class RandoTransition : IRandoItem, IRandoLocation, IRandoCouple, ILocationDependentItem
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
        

        public TempState Placed { get; set; }

        public string Name => lt.Name;

        public TempState Reachable { get; set; }

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
            lt.AddTo(pm);
        }

        public IEnumerable<Term> GetAffectedTerms()
        {
            return lt.GetAffectedTerms();
        }

        public void Place(ProgressionManager pm, ILogicDef location)
        {
            if (lt.term.Type != TermType.State) return;
            if (location is LogicTransition lt2)
            {
                pm.mu.LinkState(lt2.term, this.lt.term);
            }
            else if (location is RandoTransition rt)
            {
                pm.mu.LinkState(rt.lt.term, this.lt.term);
            }
        }
    }

}

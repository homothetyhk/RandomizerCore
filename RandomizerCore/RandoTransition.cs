using RandomizerCore.Logic;

namespace RandomizerCore
{
    public class RandoTransition : IRandoItem, IRandoLocation, IRandoCouple
    {
        public readonly LogicTransition lt;

        public RandoTransition(LogicTransition lt)
        {
            this.lt = lt;
        }

        public int sourcePriority;
        public int targetPriority;

        int IRandoItem.Priority { get => targetPriority; set => targetPriority = value; }
        int IRandoLocation.Priority { get => sourcePriority; set => sourcePriority = value; }

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
    }

}

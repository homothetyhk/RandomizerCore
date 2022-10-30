using RandomizerCore.Logic;

namespace RandomizerCore
{
    public class RandoItem : IRandoItem
    {
        public LogicItem item;
        public float Priority { get; set; }

        public string Name => item.Name;

        public TempState Placed { get ; set ; }
        public int Sphere { get; set; }
        public bool Required { get; set; }

        public void AddTo(ProgressionManager pm)
        {
            item.AddTo(pm);
        }

        public IEnumerable<Term> GetAffectedTerms()
        {
            return item.GetAffectedTerms();
        }

        public override string ToString()
        {
            return Name;
        }
    }
}

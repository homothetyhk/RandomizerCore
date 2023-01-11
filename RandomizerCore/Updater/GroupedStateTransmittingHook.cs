using RandomizerCore.Logic;
using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Updater
{
    public class GroupedStateTransmittingHook : PMHook, ILogicItem
    {
        public readonly List<Term> targets = new();
        public readonly HashSet<int> sourceLookup = new();
        private ProgressionManager pm;
        public string groupLabel;

        private StateUnion? current;
        private StateUnion? backup;

        string ILogicItem.Name => nameof(GroupedStateTransmittingHook);

        public GroupedStateTransmittingHook(string groupLabel)
        {
            this.groupLabel = groupLabel;
            this.pm = null!;
        }

        public void Add(Term source, Term target)
        {
            sourceLookup.Add(source);
            targets.Add(target);
        }

        public void AddSource(Term source)
        {
            sourceLookup.Add(source);
        }

        public void AddTarget(Term target)
        {
            targets.Add(target);
        }

        public override void Hook(ProgressionManager pm)
        {
            this.pm = pm;
            pm.AfterAddItem += Pm_AfterAddItem;
            pm.AfterAddRange += Pm_AfterAddRange;
            pm.AfterStartTemp += Pm_AfterStartTemp;
            pm.AfterEndTemp += Pm_AfterEndTemp;
            Init();
        }

        public override void Unhook(ProgressionManager pm)
        {
            pm.AfterAddItem -= Pm_AfterAddItem;
            pm.AfterAddRange -= Pm_AfterAddRange;
            pm.AfterStartTemp -= Pm_AfterStartTemp;
            pm.AfterEndTemp -= Pm_AfterEndTemp;
        }

        private void Pm_AfterEndTemp(bool obj)
        {
            if (!obj)
            {
                current = backup;
            }
        }

        private void Pm_AfterStartTemp()
        {
            backup = current;
        }

        private void Pm_AfterAddRange(IEnumerable<ILogicItem> obj)
        {
            StateUnion? after = current;
            foreach (ILogicItem item in obj)
            {
                if (ReferenceEquals(obj, this)) continue;
                foreach (Term t in item.GetAffectedTerms())
                {
                    if (sourceLookup.Contains(t))
                    {
                        StateUnion? next = pm.GetState(t);
                        if (next is null) continue;
                        else if (after is null) after = pm.GetState(t);
                        else if (StateUnion.TryUnion(after, next, out StateUnion result)) after = result;
                    }
                }
            }
            
            if (after != current)
            {
                current = after;
                BroadcastUpdate();
            }
        }

        private void Pm_AfterAddItem(ILogicItem obj)
        {
            if (ReferenceEquals(obj, this)) return; 

            StateUnion? after = current;
            foreach (Term t in obj.GetAffectedTerms())
            {
                if (sourceLookup.Contains(t))
                {
                    StateUnion? next = pm.GetState(t);
                    if (next is null) continue;
                    else if (after is null) after = pm.GetState(t);
                    else if (next is not null && StateUnion.TryUnion(after, next, out StateUnion result)) after = result;
                }
            }
            if (after != current)
            {
                current = after;
                BroadcastUpdate();
            }
        }

        private void BroadcastUpdate()
        {
            pm.Add(this);
        }

        private void Init()
        {
            foreach (int id in sourceLookup)
            {
                if (pm.GetState(id) is StateUnion s)
                {
                    current = current is null ? s : StateUnion.Union(current, s);
                }
            }
            BroadcastUpdate();
        }

        void ILogicItem.AddTo(ProgressionManager pm)
        {
            foreach (Term t in targets)
            {
                StateUnion? orig = pm.GetState(t);
                if (orig is null) pm.SetState(t, current!);
                else pm.SetState(t, StateUnion.Union(orig, current!));
            }
        }

        IEnumerable<Term> ILogicItem.GetAffectedTerms()
        {
            return targets;
        }
    }
}

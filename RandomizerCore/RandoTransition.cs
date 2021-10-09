using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;
using static RandomizerCore.LogHelper;

namespace RandomizerCore
{
    public enum OneWayType
    {
        TwoWay,
        OneWayIn,
        OneWayOut
    }

    public class RandoTransition : ILogicDef, ILogicItem
    {
        public RandoTransition(LogicTransition lt)
        {
            this.lt = lt;
        }

        


        public void Place(ProgressionManager pm)
        {
            placed = State.Temporary;
            pm.Add(this);
        }

        public bool CanGet(ProgressionManager pm)
        {
            return ((ILogicDef)lt).CanGet(pm);
        }

        public IEnumerable<Term> GetTerms()
        {
            return ((ILogicDef)lt).GetTerms();
        }

        void ILogicItem.AddTo(ProgressionManager pm)
        {
            ((ILogicItem)lt).AddTo(pm);
        }

        IEnumerable<Term> ILogicItem.GetAffectedTerms()
        {
            return lt.GetAffectedTerms();
        }

        public readonly LogicTransition lt;

        public int priority;

        public State reachable;
        public State placed;

        public bool IsSourceTransition => lt.oneWayType != OneWayType.OneWayOut;
        public bool IsTargetTransition => lt.oneWayType != OneWayType.OneWayIn;

        public int dir;
        public int targetDir;
        public bool coupled;

        public string Name => lt.Name;
    }

    public enum State
    {
        None,
        Temporary,
        Permanent
    }

    public enum TransitionState
    {
        None,
        TempGiven,
        TempFound,
        Given,
        Found,
    }

}

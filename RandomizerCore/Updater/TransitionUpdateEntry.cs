using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RandomizerCore.Logic;

namespace RandomizerCore
{
    /// <summary>
    /// The update entry for a directed transition.
    /// </summary>
    public class TransitionUpdateEntry : UpdateEntry
    {
        readonly RandoTransition source;
        readonly RandoTransition target;

        public TransitionUpdateEntry(TransitionPlacement p)
        {
            this.source = p.source;
            this.target = p.target;
        }

        public override bool CanGet(ProgressionManager pm)
        {
            return source.CanGet(pm);
        }

        public override IEnumerable<Term> GetTerms()
        {
            return ((ILogicDef)source).GetTerms().Concat(((ILogicDef)target).GetTerms());
        }

        public override void OnAdd(ProgressionManager pm)
        {
            pm.Add(source);
            pm.Add(target);
        }

        public override void OnRemove(ProgressionManager pm)
        {
            return;
        }
    }

}

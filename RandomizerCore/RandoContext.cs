using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;
using RandomizerCore.Logic;

namespace RandomizerCore
{
    public class RandoContext
    {
        public List<ItemPlacement> itemPlacements;
        public List<TransitionPlacement> transitionPlacements;
        public List<int> notchCosts;
    }
}

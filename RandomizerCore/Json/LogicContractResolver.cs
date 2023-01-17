using Newtonsoft.Json.Serialization;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    public class LogicContractResolver : DefaultContractResolver
    {
        public LogicManager? LM;

        protected override JsonObjectContract CreateObjectContract(Type objectType)
        {
            JsonObjectContract c = base.CreateObjectContract(objectType);
            if (objectType == typeof(LogicDef))
            {
                c.CreatorParameters.Add(c.Properties["Name"]);
                c.OverrideCreator = (args) => LM.GetLogicDefStrict((string)args[0]); // safe since we are modifying handling for the abstract type, which is only annotated by the custom converter
            }
            else if (objectType == typeof(LogicItem))
            {
                c.CreatorParameters.Add(c.Properties["Name"]);
                c.OverrideCreator = (args) => LM.GetItemStrict((string)args[0]); // safe since we are modifying handling for the abstract type, which is only annotated by the custom converter
            }
            else if (objectType == typeof(LogicTransition))
            {
                c.CreatorParameters.Clear();
                c.CreatorParameters.Add(c.Properties["Name"]); // if LogicObjectWriter was used
                c.CreatorParameters.Add(c.Properties["term"]); // for previous version compatibility, since Name used to be JsonIgnored
                c.OverrideCreator = (args) => LM.GetTransitionStrict(args[0] as string ?? args[1]?.ToString());
            }
            else if (objectType == typeof(LogicWaypoint))
            {
                c.CreatorParameters.Clear();
                c.CreatorParameters.Add(c.Properties["Name"]);
                c.OverrideCreator = (args) => LM.Waypoints.First(w => w.Name == (string)args[0]);
            }
            else if (objectType == typeof(DNFLogicDef))
            {
                c.CreatorParameters.Clear();
                c.CreatorParameters.Add(c.Properties["Name"]);
                c.CreatorParameters.Add(c.Properties["Logic"]);
                c.OverrideCreator = (args) => LM.CreateDNFLogicDef(new((string)args[0], (string)args[1]));
            }
#pragma warning disable CS0612 // Type or member is obsolete
            else if (objectType == typeof(OptimizedLogicDef))
            {
                c.CreatorParameters.Clear();
                c.CreatorParameters.Add(c.Properties["Name"]);
                c.CreatorParameters.Add(c.Properties["Logic"]);
                c.OverrideCreator = (args) => new OptimizedLogicDef(LM.CreateDNFLogicDef(new((string)args[0], (string)args[1])));
            }
#pragma warning restore CS0612 // Type or member is obsolete
            else if (objectType == typeof(RPNLogicDef))
            {
                c.CreatorParameters.Clear();
                c.CreatorParameters.Add(c.Properties["Name"]);
                c.CreatorParameters.Add(c.Properties["Logic"]);
                c.OverrideCreator = (args) => LM.CreateRPNLogicDef(new((string)args[0], (string)args[1]));
            }

            return c;
        }

    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using System.Reflection;
using RandomizerCore.Logic;
using RandomizerCore.Json;
using Newtonsoft.Json.Linq;

namespace RandomizerCore
{
    public static class Loader
    {
        public class LoadData
        {
            public LoadData(string path)
            {
                macros = JsonUtil.DeserializeFrom<Dictionary<string, string>>(Path.Combine(path, "macros.json")) ?? new();
                terms = JsonUtil.DeserializeFrom<string[]>(Path.Combine(path, "terms.json")) ?? Array.Empty<string>();
                unprocessedItems = JsonUtil.DeserializeArray(Path.Combine(path, "items.json"));
                locations = JsonUtil.DeserializeFrom<RawLogicDef[]>(Path.Combine(path, "locations.json")) ?? Array.Empty<RawLogicDef>();
                transitions = JsonUtil.DeserializeFrom<RawLogicTransition[]>(Path.Combine(path, "transitions.json")) ?? Array.Empty<RawLogicTransition>();
                waypoints = JsonUtil.DeserializeFrom<RawLogicDef[]>(Path.Combine(path, "waypoints.json")) ?? Array.Empty<RawLogicDef>();
            }

            /// <summary>
            /// 
            /// </summary>
            /// <param name="a">The assembly containing the embedded resources.</param>
            /// <param name="path">The path to the directory containing the resources, ending in '.'</param>
            public LoadData(Assembly a, string path)
            {
                HashSet<string> set = new(a.GetManifestResourceNames());

                macros = JsonUtil.DeserializeFrom<Dictionary<string, string>>(a, set, path + "macros.json") ?? new();
                terms = JsonUtil.DeserializeFrom<string[]>(a, set, path + "terms.json") ?? Array.Empty<string>();
                unprocessedItems = JsonUtil.DeserializeArray(a, set, path + "items.json");
                locations = JsonUtil.DeserializeFrom<RawLogicDef[]>(a, set, path + "locations.json") ?? Array.Empty<RawLogicDef>();
                waypoints = JsonUtil.DeserializeFrom<RawLogicDef[]>(a, set, path + "waypoints.json") ?? Array.Empty<RawLogicDef>();
                transitions = JsonUtil.DeserializeFrom<RawLogicTransition[]>(a, set, path + "transitions.json") ?? Array.Empty<RawLogicTransition>();
            }

            public Dictionary<string, string> macros;
            public string[] terms;
            public JArray unprocessedItems;
            public RawLogicDef[] locations;
            public RawLogicDef[] waypoints;
            public RawLogicTransition[] transitions;
            public VariableResolver vr;
        }

        public static LogicManager LoadFromDirectory(string path, VariableResolver vr = null)
        {
            return Load(new LoadData(path) 
            { 
                vr = vr ?? new VariableResolver() 
            });
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="a">The assembly containing the embedded resources.</param>
        /// <param name="resourcePath">The path to the directory containing the resources, ending in '.'</param>
        public static LogicManager LoadFromAssembly(Assembly a, string resourcePath, VariableResolver vr = null)
        {
            return Load(new LoadData(a, resourcePath)
            {
                vr = vr ?? new VariableResolver()
            });
        }

        public static LogicManager Load(LoadData ld)
        {
            LogicProcessor lp = new(ld.macros);

            return new LogicManager(
                lp,
                ld.terms,
                ld.locations,
                ld.unprocessedItems,
                ld.waypoints,
                ld.transitions,
                ld.vr);
        }
    }

}

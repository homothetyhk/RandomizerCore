using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using System.Reflection;
using RandomizerCore.Logic;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using static RandomizerCore.LogHelper;

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
                itemTemplates = JsonUtil.DeserializeFrom<LogicItemTemplate[]>(Path.Combine(path, "items.json")) ?? Array.Empty<LogicItemTemplate>();
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
                itemTemplates = JsonUtil.DeserializeFrom<LogicItemTemplate[]>(a, set, path + "items.json") ?? Array.Empty<LogicItemTemplate>();
                locations = JsonUtil.DeserializeFrom<RawLogicDef[]>(a, set, path + "locations.json") ?? Array.Empty<RawLogicDef>();
                waypoints = JsonUtil.DeserializeFrom<RawLogicDef[]>(a, set, path + "waypoints.json") ?? Array.Empty<RawLogicDef>();
                transitions = JsonUtil.DeserializeFrom<RawLogicTransition[]>(a, set, path + "transitions.json") ?? Array.Empty<RawLogicTransition>();
            }

            public Dictionary<string, string> macros;
            public string[] terms;
            public LogicItemTemplate[] itemTemplates;
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
            LogicProcessor lp = new LogicProcessor(ld.macros);

            return new LogicManager(
                lp,
                ld.terms,
                ld.locations,
                ld.itemTemplates,
                ld.waypoints,
                ld.transitions,
                ld.vr);
        }
    }

    public static class JsonUtil
    {
        public static T Deserialize<T>(JsonTextReader jtr) where T : class
        {
            try
            {
                return _js.Deserialize<T>(jtr);
            }
            catch (Exception e)
            {
                Log(e);
                return null;
            }
        }

        public static T DeserializeFrom<T>(string path) where T : class
        {
            try
            {
                if (!File.Exists(path))
                {
                    Log("File not found: " + path);
                    return null;
                }
                using StreamReader sr = File.OpenText(path);
                using JsonTextReader jtr = new(sr);
                return _js.Deserialize<T>(jtr);
            }
            catch (Exception e)
            {
                Log(e);
                return null;
            }
        }

        public static T DeserializeFrom<T>(Assembly a, HashSet<string> resourcePaths, string resourcePath) where T : class
        {
            try
            {
                if (!resourcePaths.Contains(resourcePath))
                {
                    Log("Resource not found: " + resourcePath);
                    return null;
                }
                using Stream s = a.GetManifestResourceStream(resourcePath);
                using StreamReader sr = new StreamReader(s);
                using JsonTextReader jtr = new JsonTextReader(sr);
                return _js.Deserialize<T>(jtr);
            }
            catch (Exception e)
            {
                Log(e);
                return null;
            }
        }



        private static readonly JsonSerializer _js;

        static JsonUtil()
        {
            _js = new JsonSerializer
            {
                DefaultValueHandling = DefaultValueHandling.Include,
                Formatting = Formatting.Indented,
                TypeNameHandling = TypeNameHandling.Auto,
            };

            _js.Converters.Add(new StringEnumConverter());
        }
    }

}

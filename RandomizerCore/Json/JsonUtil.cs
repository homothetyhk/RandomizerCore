using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Linq;
using RandomizerCore.Logic;
using System.Reflection;

namespace RandomizerCore.Json
{
    public static class JsonUtil
    {
        public static JsonSerializer GetLogicSerializer(LogicManager lm)
        {
            JsonSerializer js = GetNonLogicSerializer();
            js.Converters.Add(new TermConverter() { Terms = lm.Terms });
            js.Converters.Add(new StateFieldConverter() { SM = lm.StateManager });
            js.Converters.Add(new LogicObjectWriter() { LM = lm });
            js.Converters.Add(new LogicDefReader() { LM = lm });
            js.ContractResolver = new LogicContractResolver() { LM = lm };
            return js;
        }

        public static T? Deserialize<T>(JsonTextReader jtr) where T : class
        {
            return GetNonLogicSerializer().Deserialize<T>(jtr);
        }

        public static T? DeserializeFrom<T>(string path) where T : class
        {
            using StreamReader sr = File.OpenText(path);
            using JsonTextReader jtr = new(sr);
            return GetNonLogicSerializer().Deserialize<T>(jtr);
        }

        public static T? DeserializeFrom<T>(Assembly a, string resourcePath) where T : class
        {
            using Stream s = a.GetManifestResourceStream(resourcePath);
            using StreamReader sr = new(s);
            using JsonTextReader jtr = new(sr);
            return GetNonLogicSerializer().Deserialize<T>(jtr);
        }

        public static JArray DeserializeArray(Assembly a, string resourcePath)
        {
            using Stream s = a.GetManifestResourceStream(resourcePath);
            using StreamReader sr = new(s);
            using JsonTextReader jtr = new(sr);
            return JArray.Load(jtr);
        }

        public static JArray DeserializeArray(string path)
        {
            using StreamReader sr = File.OpenText(path);
            using JsonTextReader jtr = new(sr);
            return JArray.Load(jtr);
        }

        public static string Serialize(object o, Type? type = null)
        {
            JsonSerializer js = GetNonLogicSerializer();
            using StringWriter sw = new();
            if (type is null)
            {
                js.Serialize(sw, o);
            }
            else
            {
                js.Serialize(sw, o, type);
            }
            return sw.ToString();
        }

        public static string SerializeNonindented(object o, Type? type = null)
        {
            JsonSerializer js = GetNonLogicSerializer();
            js.Formatting = Formatting.None;
            using StringWriter sw = new();
            if (type is null)
            {
                js.Serialize(sw, o);
            }
            else
            {
                js.Serialize(sw, o, type);
            }
            return sw.ToString();
        }

        private static JsonSerializer GetNonLogicSerializer()
        {
            JsonSerializer js = new()
            {
                DefaultValueHandling = DefaultValueHandling.Include,
                Formatting = Formatting.Indented,
                TypeNameHandling = TypeNameHandling.Auto,
            };

            js.Converters.Add(new StringEnumConverter());
            return js;
        }
    }

}

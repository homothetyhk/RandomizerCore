using System.Reflection;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Linq;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    public static class JsonUtil
    {
        public static JsonSerializer GetLogicSerializer(LogicManager lm)
        {
            JsonSerializer js = new()
            {
                DefaultValueHandling = DefaultValueHandling.Include,
                Formatting = Formatting.Indented,
                TypeNameHandling = TypeNameHandling.Auto,
            };
            js.Converters.Add(new StringEnumConverter());
            js.Converters.Add(new TermConverter() { LM = lm });
            js.Converters.Add(new StateFieldConverter() { SM = lm.StateManager });
            LogicDefConverter.Instance.LM = lm;
            js.Converters.Add(LogicDefConverter.Instance);

            return js;
        }

        [Obsolete]
        public static JsonSerializer GetLogicSerializer(ILogicManager lm)
        {
            JsonSerializer js = new()
            {
                DefaultValueHandling = DefaultValueHandling.Include,
                Formatting = Formatting.Indented,
                TypeNameHandling = TypeNameHandling.Auto,
            };
            js.Converters.Add(new StringEnumConverter());
            js.Converters.Add(new TermConverter() { LM = lm });
            if (lm is LogicManager logicManager)
            {
                LogicDefConverter.Instance.LM = logicManager;
                js.Converters.Add(new StateFieldConverter() { SM = logicManager.StateManager });
                js.Converters.Add(LogicDefConverter.Instance);
            }

            return js;
        }

        public static T? Deserialize<T>(JsonTextReader jtr) where T : class
        {
            return _js.Deserialize<T>(jtr);
        }

        public static T? DeserializeFrom<T>(string path) where T : class
        {
            using StreamReader sr = File.OpenText(path);
            using JsonTextReader jtr = new(sr);
            return _js.Deserialize<T>(jtr);
        }

        public static T? DeserializeFrom<T>(Assembly a, string resourcePath) where T : class
        {
            using Stream s = a.GetManifestResourceStream(resourcePath);
            using StreamReader sr = new(s);
            using JsonTextReader jtr = new(sr);
            return _js.Deserialize<T>(jtr);
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
            using StringWriter sw = new();
            if (type is null)
            {
                _js.Serialize(sw, o);
            }
            else
            {
                _js.Serialize(sw, o, type);
            }
            return sw.ToString();
        }

        public static string SerializeNonindented(object o, Type? type = null)
        {
            _js.Formatting = Formatting.None;
            using StringWriter sw = new();
            if (type is null)
            {
                _js.Serialize(sw, o);
            }
            else
            {
                _js.Serialize(sw, o, type);
            }
            _js.Formatting = Formatting.Indented;
            return sw.ToString();
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

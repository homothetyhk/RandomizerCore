using System.Reflection;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Linq;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    public static class JsonUtil
    {
        public static JsonSerializer GetLogicSerializer(ILogicManager lm)
        {
            TermConverter tc = new() { LM = lm };
            LogicDefConverter ldc = new() { LM = lm };
            JsonSerializer js = new()
            {
                DefaultValueHandling = DefaultValueHandling.Include,
                Formatting = Formatting.Indented,
                TypeNameHandling = TypeNameHandling.Auto,
            };
            js.Converters.Add(new StringEnumConverter());
            js.Converters.Add(tc);
            js.Converters.Add(ldc);

            return js;
        }

        public static T Deserialize<T>(JsonTextReader jtr) where T : class
        {
            try
            {
                return _js.Deserialize<T>(jtr);
            }
            catch (Exception e)
            {
                Log(e);
                throw;
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
                throw;
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
                using StreamReader sr = new(s);
                using JsonTextReader jtr = new(sr);
                return _js.Deserialize<T>(jtr);
            }
            catch (Exception e)
            {
                Log(e);
                throw;
            }
        }

        public static JArray DeserializeArray(Assembly a, HashSet<string> resourcePaths, string resourcePath)
        {
            try
            {
                if (!resourcePaths.Contains(resourcePath))
                {
                    Log("Resource not found: " + resourcePath);
                    return null;
                }
                using Stream s = a.GetManifestResourceStream(resourcePath);
                using StreamReader sr = new(s);
                using JsonTextReader jtr = new(sr);
                return JArray.Load(jtr);
            }
            catch (Exception e)
            {
                Log(e);
                throw;
            }
        }

        public static JArray DeserializeArray(string path)
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
                return JArray.Load(jtr);
            }
            catch (Exception e)
            {
                Log(e);
                throw;
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

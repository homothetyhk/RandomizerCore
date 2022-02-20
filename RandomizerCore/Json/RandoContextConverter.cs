using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    public class RandoContextConverter : JsonConverter<RandoContext>
    {
        [ThreadStatic] public static bool inUse;
        public override bool CanWrite => !inUse;

        public override RandoContext ReadJson(JsonReader reader, Type objectType, RandoContext existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            JObject jo = JObject.Load(reader);

            LogicManager lm = jo[nameof(RandoContext.LM)].ToObject<LogicManager>(serializer);
            RandoContext ctx = (RandoContext)Activator.CreateInstance(objectType, lm);

            TermConverter tc = new() { LM = lm };
            serializer.Converters.Add(tc);
            serializer.Converters.Add(LogicDefConverter.Instance);
            LogicDefConverter.Instance.LM = lm;

            JsonReader jr = jo.CreateReader();
            serializer.Populate(jr, ctx);

            serializer.Converters.Remove(LogicDefConverter.Instance);
            serializer.Converters.Remove(tc);
            LogicDefConverter.Instance.LM = null;

            return ctx;
        }

        public override void WriteJson(JsonWriter writer, RandoContext value, JsonSerializer serializer)
        {
            TermConverter tc = new() { LM = value.LM };
            LogicDefConverter.Instance.LM = value.LM;
            serializer.Converters.Add(tc);
            serializer.Converters.Add(LogicDefConverter.Instance);

            inUse = true;
            serializer.Serialize(writer, value);
            inUse = false;

            serializer.Converters.Remove(LogicDefConverter.Instance);
            serializer.Converters.Remove(tc);

            LogicDefConverter.Instance.LM = null;
        }
    }
}

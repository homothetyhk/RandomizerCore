using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json.Serialization;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    public class RandoContextConverter : JsonConverter<RandoContext>
    {
        public override RandoContext ReadJson(JsonReader reader, Type objectType, RandoContext existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            JObject jo = JObject.Load(reader);

            LogicManager lm = jo[nameof(RandoContext.LM)].ToObject<LogicManager>(serializer);
            RandoContext ctx = (RandoContext)Activator.CreateInstance(objectType, lm);

            IContractResolver orig = serializer.ContractResolver;
            serializer.ContractResolver = new LogicContractResolver { LM = lm };

            TermConverter tc = new() { Terms = lm.Terms };
            StateFieldConverter sfc = new() { SM = lm.StateManager };
            LogicDefReader ldr = new() { LM = lm };
            serializer.Converters.Add(tc);
            serializer.Converters.Add(sfc);
            serializer.Converters.Add(ldr);

            JsonReader jr = jo.CreateReader();
            serializer.Populate(jr, ctx);

            serializer.Converters.Remove(ldr);
            serializer.Converters.Remove(sfc);
            serializer.Converters.Remove(tc);
            serializer.ContractResolver = orig;

            return ctx;
        }

        public bool skipNextWrite;
        public override bool CanWrite => !skipNextWrite || (skipNextWrite = false);

        public override void WriteJson(JsonWriter writer, RandoContext value, JsonSerializer serializer)
        {
            TermConverter tc = new() { Terms = value.LM.Terms };
            StateFieldConverter sfc = new() { SM = value.LM.StateManager };
            LogicObjectWriter low = new() { LM = value.LM };
            serializer.Converters.Add(tc);
            serializer.Converters.Add(sfc);
            serializer.Converters.Add(low);

            skipNextWrite = true;
            serializer.Serialize(writer, value);

            serializer.Converters.Remove(sfc);
            serializer.Converters.Remove(tc);
            serializer.Converters.Remove(low);
        }
    }
}

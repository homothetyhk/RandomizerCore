using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json.Serialization;
using RandomizerCore.Json;
using RandomizerCore.Logic;

namespace RandomizerCore
{
    public class RandoContextConverter : JsonConverter<RandoContext>
    {
        public override RandoContext ReadJson(JsonReader reader, Type objectType, RandoContext existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            JObject jo = JObject.Load(reader);
            RandoContext ctx = new();
            ctx.LM = jo[nameof(RandoContext.LM)].ToObject<LogicManager>(serializer);

            TermConverter tc = new() { LM = ctx.LM };
            LogicDefConverter ldc = new() { LM = ctx.LM };
            serializer.Converters.Add(tc);
            serializer.Converters.Add(ldc);

            ctx.InitialProgression = jo[nameof(ctx.InitialProgression)].ToObject<ILogicItem>(serializer);
            ctx.Vanilla = jo[nameof(ctx.Vanilla)].ToObject<List<RandoPlacement>>(serializer);
            ctx.itemPlacements = jo[nameof(ctx.itemPlacements)].ToObject<List<ItemPlacement>>(serializer);
            ctx.transitionPlacements = jo[nameof(ctx.transitionPlacements)].ToObject<List<TransitionPlacement>>(serializer);
            ctx.notchCosts = jo[nameof(ctx.notchCosts)].ToObject<List<int>>(serializer);

            serializer.Converters.Remove(ldc);
            serializer.Converters.Remove(tc);

            return ctx;
        }

        public override void WriteJson(JsonWriter writer, RandoContext value, JsonSerializer serializer)
        {
            writer.WriteStartObject();

            writer.WritePropertyName(nameof(RandoContext.LM));
            serializer.Serialize(writer, value.LM);

            TermConverter tc = new() { LM = value.LM };
            LogicDefConverter ldc = new() { LM = value.LM };
            serializer.Converters.Add(tc);
            serializer.Converters.Add(ldc);

            writer.WritePropertyName(nameof(RandoContext.InitialProgression));
            serializer.Serialize(writer, value.InitialProgression, typeof(ILogicItem));

            writer.WritePropertyName(nameof(RandoContext.Vanilla));
            serializer.Serialize(writer, value.Vanilla);

            writer.WritePropertyName(nameof(RandoContext.itemPlacements));
            serializer.Serialize(writer, value.itemPlacements);

            writer.WritePropertyName(nameof(RandoContext.transitionPlacements));
            serializer.Serialize(writer, value.transitionPlacements);

            writer.WritePropertyName(nameof(RandoContext.notchCosts));
            serializer.Serialize(writer, value.notchCosts);

            serializer.Converters.Remove(ldc);
            serializer.Converters.Remove(tc);
            writer.WriteEndObject();
        }
    }

    [JsonConverter(typeof(RandoContextConverter))]
    public class RandoContext
    {
        public LogicManager LM;
        public ILogicItem InitialProgression;
        public List<RandoPlacement> Vanilla;

        public List<ItemPlacement> itemPlacements;
        public List<TransitionPlacement> transitionPlacements;
        public List<int> notchCosts;

        public IEnumerable<GeneralizedPlacement> EnumerateExistingPlacements()
        {
            if (Vanilla != null) foreach (RandoPlacement p in Vanilla) yield return p;
            if (itemPlacements != null) foreach (ItemPlacement p in itemPlacements) yield return p;
            if (transitionPlacements != null) foreach (TransitionPlacement p in transitionPlacements) yield return p;
        }
    }
}

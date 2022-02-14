using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RandomizerCore.Logic;
using RandomizerCore.StringLogic;

namespace RandomizerCore.Json
{
    public class LMConverter : JsonConverter<LogicManager>
    {
        public override LogicManager ReadJson(JsonReader reader, Type objectType, LogicManager existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            LogicManagerBuilder lmb = new();
            JObject lm = JObject.Load(reader);
            lmb.LP = lm[nameof(LogicManager.LP)].ToObject<LogicProcessor>(serializer);
            lmb.VariableResolver = lm[nameof(LogicManager.VariableResolver)].ToObject<VariableResolver>(serializer);

            lmb.DeserializeJson(LogicManagerBuilder.JsonType.Terms, lm["Terms"]);
            lmb.DeserializeJson(LogicManagerBuilder.JsonType.Waypoints, lm["Waypoints"]);
            lmb.DeserializeJson(LogicManagerBuilder.JsonType.Transitions, lm["Transitions"]);
            lmb.DeserializeJson(LogicManagerBuilder.JsonType.Locations, lm["Logic"]);
            lmb.DeserializeJson(LogicManagerBuilder.JsonType.Items, lm["Items"]);

            return new(lmb);
        }

        public override void WriteJson(JsonWriter writer, LogicManager value, JsonSerializer serializer)
        {
            writer.WriteStartObject();

            TermConverter tc = new() { LM = value };
            LogicDefConverter.Instance.LM = value;
            serializer.Converters.Add(tc);
            serializer.Converters.Add(LogicDefConverter.Instance);

            writer.WritePropertyName("Terms");
            serializer.Serialize(writer, value.Terms);

            writer.WritePropertyName("Variables");
            serializer.Serialize(writer, value.Variables);

            writer.WritePropertyName("Logic");
            serializer.Serialize(writer, value.LogicLookup.Values.Select(l => new RawLogicDef(l.Name, l.ToInfix())));

            writer.WritePropertyName("Items");
            serializer.Serialize(writer, value.ItemLookup.Values);

            writer.WritePropertyName("Transitions");
            serializer.Serialize(writer, value.TransitionLookup.Values.Select(t => new RawLogicDef(t.Name, t.logic.ToInfix())));

            writer.WritePropertyName("Waypoints");
            serializer.Serialize(writer, value.Waypoints.Select(w => new RawLogicDef(w.Name, w.logic.ToInfix())));

            writer.WritePropertyName(nameof(value.LP));
            serializer.Serialize(writer, value.LP, typeof(LogicProcessor));

            writer.WritePropertyName(nameof(value.VariableResolver));
            serializer.Serialize(writer, value.VariableResolver, typeof(VariableResolver));
            Log($"Serialized VR as {value.VariableResolver.GetType().Name}");

            writer.WriteEndObject();
            serializer.Converters.Remove(LogicDefConverter.Instance);
            serializer.Converters.Remove(tc);

            LogicDefConverter.Instance.LM = null;
        }
    }
}

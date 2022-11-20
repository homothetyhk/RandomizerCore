using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RandomizerCore.Logic;
using RandomizerCore.Logic.StateLogic;
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
            lmb.DeserializeJson(LogicManagerBuilder.JsonType.StateFields, lm[nameof(StateManager)]);
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
            StateFieldConverter sfc = new() { SM = value.StateManager };
            LogicDefConverter.Instance.LM = value;
            serializer.Converters.Add(tc);
            serializer.Converters.Add(sfc);
            serializer.Converters.Add(LogicDefConverter.Instance);

            writer.WritePropertyName("Terms");
            serializer.Serialize(writer, value.Terms.Terms.Select((c, i) => (c, i)).ToDictionary(p => ((TermType)p.i).ToString(), p => p.c.Select(t => t.Name).ToList()));

            writer.WritePropertyName("Variables");
            serializer.Serialize(writer, value.Variables);

            writer.WritePropertyName("Logic");
            serializer.Serialize(writer, value.LogicLookup.Values.Select(l => new RawLogicDef(l.Name, l.ToInfix())));

            writer.WritePropertyName("Items");
            serializer.Serialize(writer, value.ItemLookup.Values);

            writer.WritePropertyName("Transitions");
            serializer.Serialize(writer, value.TransitionLookup.Values.Select(t => new RawLogicDef(t.Name, t.logic.ToInfix())));

            writer.WritePropertyName("Waypoints");
            serializer.Serialize(writer, value.Waypoints.Select(w => new RawWaypointDef(w.Name, w.logic.ToInfix(), stateless: value.GetTerm(w.Name).Type != TermType.State)));

            writer.WritePropertyName(nameof(value.LP));
            serializer.Serialize(writer, value.LP, typeof(LogicProcessor));

            writer.WritePropertyName(nameof(value.VariableResolver));
            serializer.Serialize(writer, value.VariableResolver, typeof(VariableResolver));

            writer.WritePropertyName(nameof(value.StateManager));
            serializer.Serialize(writer, new RawStateData(value.StateManager));

            writer.WriteEndObject();
            serializer.Converters.Remove(LogicDefConverter.Instance);
            serializer.Converters.Remove(sfc);
            serializer.Converters.Remove(tc);

            LogicDefConverter.Instance.LM = null;
        }
    }
}

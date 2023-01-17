using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RandomizerCore.Logic;
using RandomizerCore.Logic.StateLogic;
using RandomizerCore.StringLogic;

namespace RandomizerCore.Json
{
    public class LMConverter : JsonConverter<LogicManager>
    {
        public override LogicManager ReadJson(JsonReader reader, Type objectType, LogicManager? existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            LogicManagerBuilder lmb = new();
            JObject lm = JObject.Load(reader);
            lmb.LP = lm[nameof(LogicManager.LP)]!.ToObject<LogicProcessor>(serializer)!;
            lmb.VariableResolver = lm[nameof(LogicManager.VariableResolver)]!.ToObject<VariableResolver>(serializer)!;
            lmb.DeserializeJson(LogicManagerBuilder.JsonType.StateData, lm[nameof(StateManager)]!);
            lmb.DeserializeJson(LogicManagerBuilder.JsonType.Terms, lm["Terms"]!);
            lmb.DeserializeJson(LogicManagerBuilder.JsonType.Locations, lm["Logic"]!);
            try
            {
                lmb.Waypoints.UnionWith(lm["Waypoints"]!.ToObject<List<string>>()); // terms and logic are already defined
            }
            catch (Exception)
            {
                lmb.DeserializeJson(LogicManagerBuilder.JsonType.Waypoints, lm["Waypoints"]!);
            }
            try
            {
                lmb.Transitions.UnionWith(lm["Transitions"]!.ToObject<List<string>>()); // terms and logic are already defined
            }
            catch (Exception)
            {
                lmb.DeserializeJson(LogicManagerBuilder.JsonType.Transitions, lm["Transitions"]!);
            }
            lmb.DeserializeJson(LogicManagerBuilder.JsonType.Items, lm["Items"]!);

            return new(lmb);
        }

        public override void WriteJson(JsonWriter writer, LogicManager? value, JsonSerializer serializer)
        {
            if (value is null)
            {
                writer.WriteNull();
                return;
            }

            List<JsonConverter> removeConverters = new();
            for (int i = 0; i < serializer.Converters.Count; i++)
            {
                if (serializer.Converters[i] is LogicObjectWriter)
                {
                    removeConverters.Add(serializer.Converters[i]);
                    serializer.Converters.RemoveAt(i);
                    i--;
                }
            }

            writer.WriteStartObject();

            TermConverter tc = new() { Terms = value.Terms };
            StateFieldConverter sfc = new() { SM = value.StateManager };
            
            serializer.Converters.Add(tc);
            serializer.Converters.Add(sfc);

            writer.WritePropertyName("Terms");
            serializer.Serialize(writer, value.Terms.Terms.Select((c, i) => (c, i)).ToDictionary(p => ((TermType)p.i).ToString(), p => p.c.Select(t => t.Name).ToList()));

            writer.WritePropertyName("Logic");
            serializer.Serialize(writer, value.LogicLookup.Values.Select(l => new RawLogicDef(l.Name, l.InfixSource)));

            writer.WritePropertyName("Items");
            serializer.Serialize(writer, value.ItemLookup.Values);

            writer.WritePropertyName("Transitions");
            serializer.Serialize(writer, value.TransitionLookup.Keys);

            writer.WritePropertyName("Waypoints");
            serializer.Serialize(writer, value.Waypoints.Select(w => w.Name));

            writer.WritePropertyName(nameof(value.LP));
            serializer.Serialize(writer, value.LP, typeof(LogicProcessor));

            writer.WritePropertyName(nameof(value.VariableResolver));
            serializer.Serialize(writer, value.VariableResolver, typeof(VariableResolver));

            writer.WritePropertyName(nameof(value.StateManager));
            serializer.Serialize(writer, new RawStateData(value.StateManager));

            writer.WriteEndObject();
            serializer.Converters.Remove(sfc);
            serializer.Converters.Remove(tc);
            foreach (JsonConverter c in removeConverters) serializer.Converters.Add(c);
        }
    }
}

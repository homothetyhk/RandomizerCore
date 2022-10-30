using Newtonsoft.Json;
using RandomizerCore.Logic;
using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Json
{
    public class StateFieldConverter : JsonConverter<StateField>
    {
        public StateManager SM;

        public override StateField? ReadJson(JsonReader reader, Type objectType, StateField? existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            RawStateField rsf = serializer.Deserialize<RawStateField>(reader);
            return SM.FieldLookup[rsf.Name];
        }

        public override void WriteJson(JsonWriter writer, StateField? value, JsonSerializer serializer)
        {
            serializer.Serialize(writer, value.ToRawStateDef());
        }
    }
}

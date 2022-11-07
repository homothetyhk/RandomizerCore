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
            return SM.FieldLookup[serializer.Deserialize<string>(reader)];
        }

        public override void WriteJson(JsonWriter writer, StateField? value, JsonSerializer serializer)
        {
            serializer.Serialize(writer, value.Name);
        }
    }
}

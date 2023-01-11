using Newtonsoft.Json;
using RandomizerCore.Logic.StateLogic;

namespace RandomizerCore.Json
{
    public class StateFieldConverter : JsonConverter<StateField>
    {
        public StateManager? SM;

        public override StateField? ReadJson(JsonReader reader, Type objectType, StateField? existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            if (SM is null) throw new NullReferenceException(nameof(SM));
            return SM.FieldLookup[serializer.Deserialize<string>(reader)!];
        }

        public override void WriteJson(JsonWriter writer, StateField? value, JsonSerializer serializer)
        {
            if (value is null) writer.WriteNull();
            else serializer.Serialize(writer, value.Name);
        }
    }
}

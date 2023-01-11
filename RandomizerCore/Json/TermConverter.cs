using Newtonsoft.Json;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    public class TermConverter : JsonConverter<Term>
    {
        public ILogicManager? LM;

        public override Term? ReadJson(JsonReader reader, Type objectType, Term? existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            if (LM is null) throw new NullReferenceException(nameof(LM));
            return LM.GetTerm((string)reader.Value!);
        }

        public override void WriteJson(JsonWriter writer, Term? value, JsonSerializer serializer)
        {
            if (value is null) writer.WriteNull();
            else writer.WriteValue(value.Name);
        }
    }
}

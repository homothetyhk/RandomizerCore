using Newtonsoft.Json;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    public class TermConverter : JsonConverter<Term>
    {
        public TermCollection? Terms;

        public override Term? ReadJson(JsonReader reader, Type objectType, Term? existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            if (Terms is null) throw new NullReferenceException(nameof(Terms));
            if (reader.Value is null) return null;
            return Terms.GetTerm((string)reader.Value);
        }

        public override void WriteJson(JsonWriter writer, Term? value, JsonSerializer serializer)
        {
            if (value is null) writer.WriteNull();
            else writer.WriteValue(value.Name);
        }
    }
}

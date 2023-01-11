using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    public class LogicDefConverter : JsonConverter<LogicDef>
    {
        [ThreadStatic] private static LogicDefConverter? _instance;
        public static LogicDefConverter Instance { get => _instance ??= new(); }
        // the thread static instance allows the converter to be accessed from the private OptimizedLogicDef constructor, which can get called during polymorphic deserialization.

        public LogicManager? LM;

        public override LogicDef ReadJson(JsonReader reader, Type objectType, LogicDef? existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            JObject t = JObject.Load(reader);
            string? name = t.GetValue("name", StringComparison.OrdinalIgnoreCase)?.Value<string>();
            string? logic = t.GetValue("logic", StringComparison.OrdinalIgnoreCase)?.Value<string>();

            if (name is null) throw new NullReferenceException($"Error deserializing logic def at {reader.Path}: null name.");
            if (logic is null) throw new NullReferenceException($"Error deserializing logic def {name} at {reader.Path}: null logic.");
            if (LM is null) throw new NullReferenceException(nameof(LM));

            if (LM.GetLogicDef(name) is LogicDef lDef && lDef.InfixSource == logic && objectType.IsAssignableFrom(lDef.GetType()))
            {
                return lDef;
            }

            if (objectType.IsAssignableFrom(typeof(DNFLogicDef)))
            {
                return LM.CreateDNFLogicDef(new RawLogicDef(name, logic));
            }
            else if (objectType.IsAssignableFrom(typeof(RPNLogicDef)))
            {
                return LM.FromString(new RawLogicDef(name, logic));
            }
#pragma warning disable CS0612 // Type or member is obsolete
            else if (objectType.IsAssignableFrom(typeof(OptimizedLogicDef)))
            {
                return new OptimizedLogicDef(LM.CreateDNFLogicDef(new RawLogicDef(name, logic)));
            }
#pragma warning restore CS0612 // Type or member is obsolete
            else throw new NotSupportedException($"Cannot deserialize LogicDef at {reader.Path} to unexpected type {objectType.Name}");
        }

        public override bool CanWrite => false;

        public override void WriteJson(JsonWriter writer, LogicDef? value, JsonSerializer serializer)
        {
            throw new NotImplementedException();
        }
    }
}

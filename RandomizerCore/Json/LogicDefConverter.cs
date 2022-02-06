using Newtonsoft.Json;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    public class LogicDefConverter : JsonConverter<OptimizedLogicDef>
    {
        [ThreadStatic] private static LogicDefConverter _instance;
        public static LogicDefConverter Instance { get => _instance ??= new(); }
        // the thread static instance allows the converter to be accessed from the private OptimizedLogicDef constructor, which can get called during polymorphic deserialization.

        public LogicManager LM;

        public override OptimizedLogicDef ReadJson(JsonReader reader, Type objectType, OptimizedLogicDef existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            return LM.FromString(serializer.Deserialize<RawLogicDef>(reader));
        }

        public override bool CanWrite => false;

        public override void WriteJson(JsonWriter writer, OptimizedLogicDef value, JsonSerializer serializer)
        {
            throw new NotImplementedException();
        }
    }
}

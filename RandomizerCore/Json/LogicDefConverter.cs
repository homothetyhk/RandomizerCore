using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    public class LogicDefConverter : JsonConverter<OptimizedLogicDef>
    {
        public ILogicManager LM;

        public override OptimizedLogicDef ReadJson(JsonReader reader, Type objectType, OptimizedLogicDef existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            return LM.FromString(serializer.Deserialize<RawLogicDef>(reader));
        }

        public override void WriteJson(JsonWriter writer, OptimizedLogicDef value, JsonSerializer serializer)
        {
            serializer.Serialize(writer, new RawLogicDef(value.Name, value.ToInfix()));
        }
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    public class LogicDefConverter : JsonConverter<LogicDef>
    {
        public ILogicManager LM;

        public override LogicDef ReadJson(JsonReader reader, Type objectType, LogicDef existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            return LM.FromString(serializer.Deserialize<RawLogicDef>(reader));
        }

        public override void WriteJson(JsonWriter writer, LogicDef value, JsonSerializer serializer)
        {
            serializer.Serialize(writer, new RawLogicDef(value.Name, value.RawLogic));
        }
    }
}

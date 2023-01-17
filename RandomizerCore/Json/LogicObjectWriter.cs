using Newtonsoft.Json;
using RandomizerCore.Logic;

namespace RandomizerCore.Json
{
    /// <summary>
    /// Converter for objects which may have duplicate references on the LogicManager.
    /// </summary>
    public class LogicObjectWriter : JsonConverter
    {
        public LogicManager? LM;

        public override bool CanRead => false;
        public bool skipNextWrite;
        public override bool CanWrite => !skipNextWrite || (skipNextWrite = false);

        public override bool CanConvert(Type objectType)
        {
            return objectType.IsSubclassOf(typeof(LogicDef)) || objectType.IsSubclassOf(typeof(LogicItem)) || typeof(LogicTransition) == objectType || typeof(LogicWaypoint) == objectType;
        }

        public override object? ReadJson(JsonReader reader, Type objectType, object? existingValue, JsonSerializer serializer) => throw new NotImplementedException();

        public override void WriteJson(JsonWriter writer, object? value, JsonSerializer serializer)
        {
            if (value is null)
            {
                writer.WriteNull();
            }
            else if (value is LogicDef ld)
            {
                if (ReferenceEquals(ld, LM.GetLogicDef(ld.Name)))
                {
                    writer.WriteStartObject();
                    writer.WritePropertyName("$type");
                    writer.WriteValue("RandomizerCore.Logic.LogicDef, RandomizerCore");
                    writer.WritePropertyName("Name");
                    writer.WriteValue(ld.Name);
                    writer.WriteEndObject();
                }
                else
                {
                    skipNextWrite = true;
                    serializer.Serialize(writer, value, typeof(LogicDef));
                }
            }
            else if (value is LogicItem li)
            {
                if (ReferenceEquals(li, LM.GetItem(li.Name)))
                {
                    writer.WriteStartObject();
                    writer.WritePropertyName("$type");
                    writer.WriteValue("RandomizerCore.LogicItem, RandomizerCore");
                    writer.WritePropertyName("Name");
                    writer.WriteValue(li.Name);
                    writer.WriteEndObject();
                }
                else
                {
                    skipNextWrite = true;
                    serializer.Serialize(writer, value, typeof(LogicItem));
                }
            }
            else if (value is LogicTransition lt)
            {
                if (ReferenceEquals(lt, LM.GetTransition(lt.Name)))
                {
                    writer.WriteStartObject();
                    writer.WritePropertyName("$type");
                    writer.WriteValue("RandomizerCore.Logic.LogicTransition, RandomizerCore");
                    writer.WritePropertyName("Name");
                    writer.WriteValue(lt.Name);
                    writer.WriteEndObject();
                }
                else
                {
                    skipNextWrite = true;
                    serializer.Serialize(writer, value, typeof(LogicTransition));
                }
            }
            else if (value is LogicWaypoint lw)
            {
                if (ReferenceEquals(lw.logic, LM.GetLogicDef(lw.Name)))
                {
                    writer.WriteStartObject();
                    writer.WritePropertyName("$type");
                    writer.WriteValue("RandomizerCore.Logic.LogicWaypoint, RandomizerCore");
                    writer.WritePropertyName("Name");
                    writer.WriteValue(lw.Name);
                    writer.WriteEndObject();
                }
                else
                {
                    skipNextWrite = true;
                    serializer.Serialize(writer, value, typeof(LogicWaypoint));
                }
            }
            else
            {
                skipNextWrite = true;
                serializer.Serialize(writer, value);
            }
        }
    }
}

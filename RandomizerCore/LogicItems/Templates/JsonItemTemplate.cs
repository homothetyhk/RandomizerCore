using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RandomizerCore.Json;
using RandomizerCore.Logic;

namespace RandomizerCore.LogicItems.Templates
{
    /// <summary>
    /// Item template wrapper for a json representation of an item.
    /// </summary>
    public record JsonItemTemplate : ILogicItemTemplate
    {
        public JsonItemTemplate(string json) : this(JToken.Parse(json)) { }

        public JsonItemTemplate(JToken t)
        {
            Name = t.Value<string>("Name");
            JToken = t;
        }

        public string Name { get; }
        public JToken JToken { get; }

        public LogicItem Create(LogicManager lm)
        {
            JsonSerializer js = JsonUtil.GetLogicSerializer(lm);
            return JToken.ToObject<LogicItem>(js);
        }
    }
}

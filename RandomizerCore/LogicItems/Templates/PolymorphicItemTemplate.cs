using RandomizerCore.Logic;
using System.Reflection;

namespace RandomizerCore.LogicItems.Templates
{
    /// <summary>
    /// A reflection-based item template.
    /// </summary>
    public class PolymorphicItemTemplate : ILogicItemTemplate
    {
        /// <summary>
        /// The "Name" entry in Properties.
        /// </summary>
        public string Name { get => (string)Properties["Name"]; }
        /// <summary>
        /// A case-insenstive collection of properties for the item.
        /// </summary>
        public Dictionary<string, object> Properties { get; } = new(StringComparer.InvariantCultureIgnoreCase);
        /// <summary>
        /// The type of item to instantiate.
        /// </summary>
        public Type ItemType { get; set; }
        /// <summary>
        /// The constructor to use to instantiate the item. Can be omitted if ItemType is set, in which case the template will look for a constructor which takes a single string parameter.
        /// </summary>
        public ConstructorInfo Constructor { get; set; }

        public void SetProperty(string key, object value) => Properties[key] = value;

        public LogicItem Create(LogicManager lm)
        {
            if (Constructor is null)
            {
                Constructor = ItemType.GetConstructor(new Type[] { typeof(string) });
            }
            LogicItem item = (LogicItem)Constructor.Invoke(Constructor.GetParameters().Select(pi => Properties[pi.Name]).ToArray());
            foreach (var kvp in Properties)
            {
                if (ItemType.GetProperty(kvp.Key, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance) is PropertyInfo pi && pi.CanWrite)
                {
                    pi.SetValue(item, kvp.Value);
                }
            }
            return item;
        }
    }
}

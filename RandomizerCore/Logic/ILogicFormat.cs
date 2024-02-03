using RandomizerCore.Logic.StateLogic;
using RandomizerCore.LogicItems;
using RandomizerCore.StringItems;
using System.ComponentModel;

namespace RandomizerCore.Logic
{
    [EditorBrowsable(EditorBrowsableState.Never)]
    public static class LogicFormatExtentions
    {
        /// <summary>
        /// Returns the result of the ILogicFormat method corresponding to the LogicFileType.
        /// </summary>
        public static object LoadFile(this ILogicFormat fmt, LogicFileType type, Stream s)
        {
            return type switch
            {
                LogicFileType.Terms => fmt.LoadTerms(s),
                LogicFileType.Waypoints => fmt.LoadWaypoints(s),
                LogicFileType.Transitions => fmt.LoadTransitions(s),
                LogicFileType.Macros => fmt.LoadMacros(s),
                LogicFileType.Items => fmt.LoadItems(s),
                LogicFileType.Locations => fmt.LoadLocations(s),
                LogicFileType.LogicEdit => fmt.LoadLogicEdits(s),
                LogicFileType.MacroEdit => fmt.LoadMacroEdits(s),
                LogicFileType.LogicSubst => fmt.LoadLogicSubstitutions(s),
                LogicFileType.ItemTemplates => fmt.LoadItemTemplates(s),
                LogicFileType.StateData => fmt.LoadStateData(s),
                LogicFileType.ItemStrings => fmt.LoadItemStrings(s),
                _ => throw new NotImplementedException("Unrecognized logic format " + type),
            };
        }

        /// <summary>
        /// Converts the string to a stream, and returns the result of the ILogicFormat method corresponding to the LogicFileType.
        /// </summary>
        public static object LoadFile(this ILogicFormat fmt, LogicFileType type, string s)
        {
            byte[] data = System.Text.Encoding.Unicode.GetBytes(s);
            using MemoryStream ms = new(data);
            using StreamReader sr = new(ms, System.Text.Encoding.Unicode);
            return fmt.LoadFile(type, s);
        }
    }

    /// <summary>
    /// Interface describing a strategy for loading a specific file format into a <see cref="LogicManagerBuilder"/>.
    /// </summary>
    /// <seealso cref="LogicManagerBuilder.DeserializeFile(LogicFileType, ILogicFormat, Stream)"/>
    public interface ILogicFormat
    {
        /// <summary>
        /// Loads a collection of terms from a file.
        /// </summary>
        IEnumerable<(string, TermType)> LoadTerms(Stream s);
        /// <summary>
        /// Loads a collection of waypoints from a file.
        /// </summary>
        IEnumerable<RawWaypointDef> LoadWaypoints(Stream s);
        /// <summary>
        /// Loads a collection of transitions from a file.
        /// </summary>
        IEnumerable<RawLogicDef> LoadTransitions(Stream s);
        /// <summary>
        /// Loads a collection of macros from a file.
        /// </summary>
        Dictionary<string, string> LoadMacros(Stream s);
        /// <summary>
        /// Loads a collection of items from a file. In general, some items cannot be constructed with a <see cref="LogicManager"/>. 
        /// <br/>It is the repsonsibility of format implementations to forbid such items or wrap them in an appropriate template.
        /// </summary>
        IEnumerable<ILogicItemTemplate> LoadItems(Stream s);
        /// <summary>
        /// Loads a collection of locations from a file.
        /// </summary>
        IEnumerable<RawLogicDef> LoadLocations(Stream s);
        /// <summary>
        /// Loads a collection of logic edits from a file.
        /// </summary>
        IEnumerable<RawLogicDef> LoadLogicEdits(Stream s);
        /// <summary>
        /// Loads a collection of macro edits from a file.
        /// </summary>
        Dictionary<string, string> LoadMacroEdits(Stream s);
        /// <summary>
        /// Loads a collection of logic substitutions from a file.
        /// </summary>
        IEnumerable<RawSubstDef> LoadLogicSubstitutions(Stream s);
        /// <summary>
        /// Loads a collection of item templates from a file.
        /// </summary>
        IEnumerable<ILogicItemTemplate> LoadItemTemplates(Stream s);
        /// <summary>
        /// Loads a collection of item strings from a file.
        /// </summary>
        IEnumerable<StringItemTemplate> LoadItemStrings(Stream s);

        /// <summary>
        /// Loads state data from a file.
        /// </summary>
        RawStateData LoadStateData(Stream s);
    }
}

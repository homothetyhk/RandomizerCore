using RandomizerCore.Logic.StateLogic;
using RandomizerCore.LogicItems;
using RandomizerCore.StringItems;

namespace RandomizerCore.Logic
{
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

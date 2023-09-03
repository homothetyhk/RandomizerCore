using RandomizerCore.Logic.StateLogic;
using RandomizerCore.LogicItems;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Logic
{
    /// <summary>
    /// Interface describing a strategy for loading a specific file format into a <see cref="LogicManagerBuilder"/>.
    /// </summary>
    /// <seealso cref="LogicManagerBuilder.DeserializeFile(LogicManagerBuilder.JsonType, ILogicFormat, Stream)"/>
    public interface ILogicFormat
    {
        /// <summary>
        /// Loads a collection of terms from a file.
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
        /// Loads a collection of items from a file.
        /// </summary>
        /// <returns>
        ///     An IEnumerable which may contain any mix of <see cref="LogicItem"/>s and <see cref="ILogicItemTemplate"/>s.
        ///     It is the responsibility of format implementations to either forbid the use of items which cannot be constructed
        ///     without a <see cref="LogicManager"/>, or implicitly construct them as <see cref="ILogicItemTemplate"/>s.
        /// </returns>
        IEnumerable<object> LoadItems(Stream s);
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
        /// Loads state data from a file.
        /// </summary>
        RawStateData LoadStateData(Stream s);
    }
}

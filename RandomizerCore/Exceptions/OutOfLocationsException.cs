namespace RandomizerCore.Exceptions
{
    /// <summary>
    /// Exception which indicates that an operation was performed without sufficient available locations.
    /// </summary>
    public class OutOfLocationsException : Exception
    {
        public OutOfLocationsException() { }
        public OutOfLocationsException(string message) : base(message) { }
        public OutOfLocationsException(string message, Exception inner) : base(message, inner) { }
    }
}

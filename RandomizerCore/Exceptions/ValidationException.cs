namespace RandomizerCore.Exceptions
{
    /// <summary>
    /// Exception raised during the validation stage of the randomizer.
    /// </summary>
    public class ValidationException : Exception
    {
        public ValidationException() { }
        public ValidationException(string message) : base(message) { }
        public ValidationException(string message, Exception inner) : base(message, inner) { }
    }
}

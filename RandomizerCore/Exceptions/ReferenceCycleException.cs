namespace RandomizerCore.Exceptions
{
    /// <summary>
    /// Exception thrown when a cyclic reference is detected in logic. Prevents computations that would lead to infinite recursion/stack overflow.
    /// </summary>
    public class ReferenceCycleException : Exception
    {
        public ReferenceCycleException() { }
        public ReferenceCycleException(string message) : base(message) { }
        public ReferenceCycleException(string message, Exception inner) : base(message, inner) { }
    }
}

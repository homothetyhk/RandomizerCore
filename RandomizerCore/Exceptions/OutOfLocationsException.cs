using System;

namespace RandomizerCore.Exceptions
{
    public class OutOfLocationsException : Exception
    {
        public OutOfLocationsException() { }
        public OutOfLocationsException(string message) : base(message) { }
        public OutOfLocationsException(string message, Exception inner) : base(message, inner) { }
    }
}

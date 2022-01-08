namespace RandomizerCore
{
    public enum RandoEventType
    {
        Info,
        Initializing,
        NewAttempt,
        Validating,
        Finished,
        Error,
    }

    /// <summary>
    /// Class which handles events for the Randomizer class.
    /// </summary>
    public class RandoMonitor
    {
        public event Action<RandoEventType, string> OnSendEvent;
        public event Action OnNewAttempt;
        public event Action<Exception> OnError;

        public void SendError(Exception e)
        {
            OnError?.Invoke(e);
            SendEvent(RandoEventType.Error, e?.ToString());
        }

        public void SendEvent(RandoEventType type, string message = null)
        {
            if (type == RandoEventType.NewAttempt) OnNewAttempt?.Invoke();
            OnSendEvent?.Invoke(type, message);
        }

        public void SendMessage(string message) => SendEvent(RandoEventType.Info, message);
    }
}

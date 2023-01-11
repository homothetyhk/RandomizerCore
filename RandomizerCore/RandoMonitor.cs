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
        BeginStage,
    }

    /// <summary>
    /// Class which handles events for the Randomizer class.
    /// </summary>
    public class RandoMonitor
    {
        public event Action<RandoEventType, string?>? OnSendEvent;
        public event Action? OnNewAttempt;
        public event Action<Exception>? OnError;

        public (int StageIndex, TempState State)? Stage { get; private set; }
        public (int StageIndex, TempState State)? PreviousStage { get; private set; }

        public void SendError(Exception e)
        {
            SendEvent(RandoEventType.Error, e.ToString());
            OnError?.Invoke(e);
        }

        public void SendBeginStage(string stageLabel, int stageIndex, TempState state) 
        {
            PreviousStage = Stage;
            Stage = (stageIndex, state);
            SendEvent(RandoEventType.BeginStage, $"{stageLabel} ({state})");
        }

        public void SendEvent(RandoEventType type, string? message = null)
        {
            OnSendEvent?.Invoke(type, message);
            if (type == RandoEventType.NewAttempt)
            {
                Stage = PreviousStage = null;
                OnNewAttempt?.Invoke();
            }
        }

        public void SendMessage(string message) => SendEvent(RandoEventType.Info, message);
    }
}

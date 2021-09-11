using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
    /// 
    /// </summary>
    public class RandoMonitor
    {
        public event Action<RandoEventType, string> OnSendEvent;

        public virtual void SendEvent(RandoEventType type, string message = null)
        {
            OnSendEvent?.Invoke(type, message);
        }

        public virtual void SendMessage(string message) => SendEvent(RandoEventType.Info, message);
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RandomizerCore.Logic
{
    public class Term
    {
        public Term(int Id, string Name)
        {
            this.Id = Id;
            this.Name = Name;
        }

        public readonly int Id = int.MinValue;
        public readonly string Name;
        public override string ToString() => Name;

        public static implicit operator int(Term term)
        {
            return term.Id;
        }
    }
}

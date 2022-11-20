using System.Text;

namespace RandomizerCore.Extensions
{
    public static class StringExtensions
    {
        /// <summary>
        /// Returns a consistent platform-independent hash code for the string.
        /// </summary>
        public static int GetStableHashCode(this string str)
        {
            unchecked
            {
                int hash1 = 5381;
                int hash2 = hash1;

                for (int i = 0; i < str.Length && str[i] != '\0'; i += 2)
                {
                    hash1 = ((hash1 << 5) + hash1) ^ str[i];
                    if (i == str.Length - 1 || str[i + 1] == '\0')
                        break;
                    hash2 = ((hash2 << 5) + hash2) ^ str[i + 1];
                }

                return hash1 + (hash2 * 1566083941);
            }
        }

        /// <summary>
        /// Converts a "camelCase" string to "Camel Case".
        /// </summary>
        public static string FromCamelCase(this string str)
        {
            StringBuilder uiname = new StringBuilder(str);
            if (str.Length > 0)
            {
                uiname[0] = char.ToUpper(uiname[0]);
            }

            for (int i = 1; i < uiname.Length; i++)
            {
                if (char.IsUpper(uiname[i]))
                {
                    if (char.IsLower(uiname[i - 1]))
                    {
                        uiname.Insert(i++, " ");
                    }
                    else if (i + 1 < uiname.Length && char.IsLower(uiname[i + 1]))
                    {
                        uiname.Insert(i++, " ");
                    }
                }

                if (char.IsDigit(uiname[i]) && !char.IsDigit(uiname[i - 1]) && !char.IsWhiteSpace(uiname[i - 1]))
                {
                    uiname.Insert(i, " ");
                }
            }

            return uiname.ToString();
        }

        /// <summary>
        /// If the string can be parsed to a value of the enum type, ignoring case, returns true and outputs the value. Otherwise, returns false.
        /// </summary>
        public static bool TryToEnum<T>(this string self, out T val) where T : Enum
        {
            try
            {
                val = (T)Enum.Parse(typeof(T), self, ignoreCase: true);
                return true;
            }
            catch
            {
                val = default;
                return false;
            }
        }
    }
}

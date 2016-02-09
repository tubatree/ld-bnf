using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

namespace splitter
{
    public class Slugger
    {
        readonly ConcurrentDictionary<string, string> Cache = new ConcurrentDictionary<string, string>();

        static string ConcatSpaces(string toSlug)
        {
            return Regex.Replace(toSlug, @"\s+", " ");
        }

        static bool IsEntity(char ch)
        {
            var unicodeCategory = CharUnicodeInfo.GetUnicodeCategory(ch);

            return unicodeCategory != UnicodeCategory.LowercaseLetter &&
                   unicodeCategory != UnicodeCategory.UppercaseLetter &&
                   unicodeCategory != UnicodeCategory.DecimalDigitNumber &&
                   unicodeCategory != UnicodeCategory.OtherNumber &&
                   unicodeCategory != UnicodeCategory.SpaceSeparator &&
                   ch != '-';
        }

        static string CullEntities(string toSlug)
        {
            var builder = new StringBuilder();

            foreach (var ch in toSlug.Where(ch => !IsEntity(ch)))
            {
                builder.Append(ch);
            }

            return builder.ToString();
        }

        public string For(string toSlug, IEnumerable<string> slugs)
        {
            toSlug = string.IsNullOrEmpty(toSlug) ? "unnamed" : toSlug;

            if (Cache.ContainsKey(toSlug))
                return EnsureSlugUnique(Cache[toSlug], slugs);

            var slug = ConcatSpaces(CullEntities(toSlug.ToLower().Trim()))
                .Replace(" ", "-");

            Cache[toSlug] = slug;

            slug = EnsureSlugUnique(slug, slugs);

            return slug;
        }

        static string EnsureSlugUnique(string newSlug, IEnumerable<string> slugs)
        {
            if (!slugs.Any(s => s == newSlug))
                return newSlug;

            var numberSuffix = 2;
            do
            {
                var slugAttempt = newSlug + "-" + numberSuffix;
                if (!slugs.Contains(slugAttempt))
                    return slugAttempt;
                numberSuffix++;
            } while (numberSuffix < 1000);

            throw new Exception("Unique slug could not be identified.");
        }

        readonly List<string> slugs = new List<string>(); 

        public string For(string toSlug, bool unique = true)
        {
            if (!unique)
                return For(toSlug, new string[] {});

            var result = For(toSlug, slugs);
            slugs.Add(result);
            return result;
        }
    }
}

package org.barend.babelmagic

// Returns true if the character counts as punctuation
// Punctuations are used to split words (make word alignment better),
// and to codify texts (two characters differing only in punctuation are not that bad)
// NOTE: don't put any character used as aligner ("*" or "#") here
fun isPunctuation(c : Char): Boolean
{
    return c == '.' || c == ','
        || c == ':' || c == ';'
        || c == '?' || c == '!' || c == '¿' || c == '¡'
        || c == '"' || c == '\'' || c == '«' || c == '»' || c == '„' || c == '“'
        || c == '–' || c == '—' || c == '-' || c == '-' || c == '~'
        || c == ' '
}

fun isPunctuation(c : EnrichedChar): Boolean
{
    return c.code != ENRICHED_CHAR_ALIGNMENT && isPunctuation(c.c)
}

package org.barend.babelmagic

const val SIMILARITY_UNKNOWN = 0
const val SIMILARITY_EQUAL = 18
const val SIMILARITY_CASE = 16
const val SIMILARITY_ACCENT = 14
const val SIMILARITY_PUNCTUATION = 12
const val SIMILARITY_DIFFERENT = -1
const val SIMILARITY_MIX = -2

// Only used in extended similarity, not for alignments
const val SIMILARITY_MOVED = -3

// Provide a similarity function for Needleman Wunsch alignment.
// S(a, b) is the similarity of characters a and b.
// The higher the result value, the more similar they are.
// The returned value is also used for coloring the text later.
// Also for that reason, it should be unique for different situations.
fun getSimilarity(s1 : Char, s2 : Char) : Int
{
    if (s1 == s2)
    {
        return SIMILARITY_EQUAL
    } else if (isPunctuation(s1) && isPunctuation(s2))
    {
        return SIMILARITY_PUNCTUATION
    } else if (isPunctuation(s1) || isPunctuation(s2))
    {
        // Consider as different, with less binding than normal differences
        //println("Compare ${s1} with ${s2}")
        return SIMILARITY_MIX
    }

    // For all code below diacritics are removed with unaccent
    val u1 = s1.unaccent()
    val u2 = s2.unaccent()

    // TODO: we could also make a distinction vowel/consonant for better alignment

    return when
    {
        u1 == u2 -> SIMILARITY_ACCENT
        u1.lowercase() == u2.lowercase() -> SIMILARITY_CASE
        else -> SIMILARITY_DIFFERENT
    }
}

// Returns a value between 0.0 and 1.0. 1.0 = 100% similar, 0 = totally different
fun getStringSimilarityScore(s1 : String, s2 : String) : Double
{
    if (s1.isEmpty() || s2.isEmpty())
    {
        return 0.0
    }
    val aligned = alignWithNeedlemanWunsch(EnrichedString(s1), EnrichedString(s2))
    val differenceCount = getDifferencesInAlignedTexts(aligned[0], aligned[1])
    return 1.0 - differenceCount / s1.length.toDouble()
}

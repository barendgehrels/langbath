package org.barend.babelmagic

data class SplitAlignment(val text : String, val code : Int)
{
    override fun toString() : String = "${code}: <${text}>"
}

private fun getExtendedSimilarity(s1 : EnrichedChar, s2 : EnrichedChar) : Int
{
    var result = getSimilarity(s1.c, s2.c)

    if (result == SIMILARITY_MIX && (s1.code == ENRICHED_CHAR_ALIGNMENT || s2.code == ENRICHED_CHAR_ALIGNMENT))
    {
        // The wrong char is marked as "aligned", the other is MIX (=punctuation + something)
        // This still counts as punctuation
        return SIMILARITY_PUNCTUATION
    }

    if (result == SIMILARITY_EQUAL && s1.code == ENRICHED_CHAR_MOVED) {
        result = SIMILARITY_MOVED
    }
    if (s1.code == ENRICHED_CHAR_EXTRA || s1.code == ENRICHED_CHAR_WRONG_WORD) {
        result = SIMILARITY_DIFFERENT
    }
    return result
}

// Split text, depending on if the text is equal,
// a character is wrong, a word is wrong or punctuation is wrong, etc.
fun splitAlignment(text1 : EnrichedString, text2 : EnrichedString) : List<SplitAlignment>
{
    val result = mutableListOf<SplitAlignment>()

    if (text1.length != text2.length)
    {
        return result
    }

    var similarity = SIMILARITY_UNKNOWN
    var begin = 0
    for (i in 0 until text2.length)
    {
        val newSimilarity = getExtendedSimilarity(text1.getChar(i), text2.getChar(i))
        if (similarity != newSimilarity)
        {
            if (i != begin)
            {
                result.add(SplitAlignment(text2.substring(begin, i), similarity))
                begin = i
            }
            similarity = newSimilarity
        }
    }
    result.add(SplitAlignment(text2.substring(begin, text2.length), similarity))

    return result
}

// The reverse of splitting: recreate the (correct, = second) input of the split function
fun splitAlignmentToString(list : List<SplitAlignment>) : String
{
    var result = ""
    for (s in list)
    {
        result += s.text
    }
    return result
}

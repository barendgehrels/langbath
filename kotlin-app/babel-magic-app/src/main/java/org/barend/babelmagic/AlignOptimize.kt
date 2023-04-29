// Changes aligned texts a bit.

package org.barend.babelmagic

private fun basicSkip(a : EnrichedString, b : EnrichedString, index : Int) : Boolean
{
    val ac = a.getChar(index)
    val bc = b.getChar(index)

    return bc.code == ENRICHED_CHAR_ALIGNMENT
            && ac.code == ENRICHED_CHAR_WRONG_WORD // || isPunctuation(ac))
}

// Example: wrong="tow", correct="two". This aligns to "t*ow" and "two*"
// Because 75% is then wrong, the whole word is marked wrong. But it's then weird to display the last "*".
// Therefore that one is omitted.
// So conditions are:
// - the correct should be marked as alignment
// - the wrong   should be marked as wrong word, or space
// - only mark the end of a wrong word.
//   if the next is Extra
private fun skip(a : EnrichedString, b : EnrichedString, index : Int) : Boolean
{
    if (! basicSkip(a, b, index))
    {
        return false
    }

    if (index + 1 < a.length)
    {
        val next = a.getChar(index + 1)
        if (next.code == ENRICHED_CHAR_EXTRA)
        {
            // It is followed by an extra character. Then don't skip
            return false
        }
        if (next.code == ENRICHED_CHAR_WRONG_WORD && !basicSkip(a, b, index + 1))
        {
            // The next char is still marked as wrong, but would not be skipped.
            // Then don't skip this either.
            return false
        }
    }

    return true
}

// After alignment, some characters (aligners in corrected text) can be removed for better display.
fun optimizeAlignment(
    a : EnrichedString, b : EnrichedString
) : Array<EnrichedString>
{
    if (a.length != b.length)
    {
        return emptyArray()
    }
    val resultA = EnrichedString()
    val resultB = EnrichedString()
    for (i in a.indices)
    {
        if (!skip(a, b, i))
        {
            resultA.append(a.getChar(i))
            resultB.append(b.getChar(i))
        }
    }
    return arrayOf(resultA, resultB)
}


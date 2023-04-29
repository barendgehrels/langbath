// Marks errors (word by word) in aligned texts.

package org.barend.babelmagic

private fun appendPart(
    text1 : EnrichedString,
    text2 : EnrichedString,
    begin : Int,
    end : Int,
    charCount : Int,
    errorCount : Int,
) : EnrichedString
{
    var result = EnrichedString()
    if (charCount <= 0)
    {
        return result
    }

    // Because short texts should not be counted wrong completely too easily,
    // 1 is added.
    // For example:
    //   * me/m' -> 1/3 is OK
    //   * blue/bloo -> 2/5 is OK
    //   * bread/brown -> 3/6 is not OK
    //   * please/plz -> pl***** -> 4/7 is not OK
    val errorRate = errorCount.toFloat() / (charCount.toFloat() + 1.0f)
    val replace = errorRate >= 0.5f

    var isExtra = true
    for (i in begin until end)
    {
        if (text2.getChar(i).code != ENRICHED_CHAR_ALIGNMENT)
        {
            isExtra = false
        }
    }
    for (i in begin until end)
    {
        when
        {
            isExtra && replace -> result.append(text1[i], ENRICHED_CHAR_EXTRA)
            replace -> result.append(text1[i], ENRICHED_CHAR_WRONG_WORD)
            else -> result.append(text1.getChar(i))
        }
    }

    //println("CORRECTION ${errorRate} -> '${result}'")
    return result
}


// Adapts the first aligned text. If most (>50%) of the word is wrong, then set the whole word to wrong,
// using the _SCRATCHED code
// The length of both input texts should be the same.
// The output length will have the same length too.
// Example: input {"this is wrong", "this is right"}, output {"this is [w:wrong]", "this is right"}
fun alignMarkWords(text1 : EnrichedString, text2 : EnrichedString) : EnrichedString
{
    if (text1.length != text2.length)
    {
        return EnrichedString()
    }

    val low1 = text1.toString().lowercase()
    val low2 = text2.toString().lowercase()

    var result = EnrichedString()
    var begin = 0
    var charCount = 0
    var errorCount = 0
    for (i in 0 until text1.length)
    {
        val c1 = text1.getChar(i)
        val c2 = text2.getChar(i)
        if (isPunctuation(c1) || isPunctuation(c2))
        {
            result.append(appendPart(text1, text2, begin, i, charCount, errorCount))
            result.append(c1)
            charCount = 0
            errorCount = 0
            begin = i + 1
        }
        else
        {
            charCount++
            if (low1[i].unaccent() != low2[i].unaccent())
            {
                errorCount += 1
            }
        }
    }
    result.append(appendPart(text1, text2, begin, text1.length, charCount, errorCount))
    assert(text1.length == result.length)
    return result
}


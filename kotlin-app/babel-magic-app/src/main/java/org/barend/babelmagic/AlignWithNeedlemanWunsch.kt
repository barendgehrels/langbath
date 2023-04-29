// Implementation of the Needleman-Wunsch algorithm to align input
// such that they have the same length, with similar parts at similar places.

package org.barend.babelmagic

import java.lang.Integer.max

private const val DEBUG_ALIGN_NEEDLEMAN_WUNSCH = false

// Return two strings of equal length based on two input strings.
// For example: input="panther", "panter" it will return "panther", "pant*er"
// See https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm
// The algorithm below follows the pseudo code quite literally including variable names
// The function S is here called Similarity
// The gap score d is here called gapScore
fun alignWithNeedlemanWunsch(
    a : EnrichedString, b : EnrichedString, aligner : Char = '*', gapScore : Int = -5
) : Array<EnrichedString>
{
    val lenA = a.length
    val lenB = b.length
    val f = Array<Array<Int>>(lenA + 1, { Array<Int>(lenB + 1, { 0 }) })

    for (i in 0..lenA)
    {
        f[i][0] = gapScore * i
    }
    for (j in 0..lenB)
    {
        f[0][j] = gapScore * j
    }

    for (i in 1..lenA)
    {
        val ai = a[i - 1]
        for (j in 1..lenB)
        {
            val match = f[i - 1][j - 1] + getSimilarity(ai, b[j - 1])
            val delete = f[i - 1][j] + gapScore
            val insert = f[i][j - 1] + gapScore
            f[i][j] = max(match, max(delete, insert))
        }
    }

    // Fill the result strings. They are filled from end to begin.
    val alignmentA = EnrichedString()
    val alignmentB = EnrichedString()
    var i = lenA
    var j = lenB
    while (i > 0 || j > 0)
    {
        if (i > 0 && j > 0 && f[i][j] == f[i - 1][j - 1] + getSimilarity(a[i - 1], b[j - 1]))
        {
            alignmentA.prepend(a.getChar(i - 1))
            alignmentB.prepend(b.getChar(j - 1))
            i--
            j--
        } else if (i > 0 && f[i][j] == f[i - 1][j] + gapScore)
        {
            alignmentA.prepend(a.getChar(i - 1))
            alignmentB.prepend(EnrichedChar(aligner, ENRICHED_CHAR_ALIGNMENT))
            i--
        } else
        {
            assert(j > 0)
            alignmentA.prepend(EnrichedChar(aligner, ENRICHED_CHAR_ALIGNMENT))
            alignmentB.prepend(b.getChar(j - 1))
            j--
        }
    }

    if (DEBUG_ALIGN_NEEDLEMAN_WUNSCH)
    {
        println(" MATRIX")
        for (di in 0..lenA)
        {
            for (dj in 0..lenB)
            {
                print(" " + (f[di])[dj])
            }
            println(" END ROW")
        }
        println(" END MATRIX")
        println("A:$alignmentA")
        println("B:$alignmentB")
    }

    return arrayOf(alignmentA, alignmentB)
}

fun getDifferencesInAlignedTexts(text1 : EnrichedString, text2 : EnrichedString) : Int
{
    if (text1.length != text2.length)
    {
        return max(text1.length, text2.length)
    }
    var sumDifferences = 0
    for (i in 0 until text1.length)
    {
        if (text1[i] != text2[i])
        {
            sumDifferences++
        }
    }
    return sumDifferences
}

package org.barend.babelmagic

// Provide versions not working with EnrichedString, but with String directly
// Only available for tests

fun alignWithNeedlemanWunsch(
    a : String, b : String, aligner : Char = '*', gapScore : Int = -5
) : Array<String>
{
    var ea = EnrichedString(a)
    var eb = EnrichedString(b)
    println("EA: ${ea}, EB: ${eb}")
    val result = alignWithNeedlemanWunsch(ea, eb, aligner, gapScore)
    return arrayOf(result[0].toString(), result[1].toString())
}

fun getDifferencesInAlignedTexts(text1 : String, text2 : String) : Int
{
    return getDifferencesInAlignedTexts(EnrichedString(text1), EnrichedString(text2))
}

fun alignAdaptWords(text1 : String, text2 : String) : String
{
    return alignMarkWords(EnrichedString(text1), EnrichedString(text2)).toString()
}

fun optimizeAlignment(
    a : String, b : String
) : Array<String>
{
    val result = optimizeAlignment(EnrichedString(a), EnrichedString(b))
    return arrayOf(result[0].toString(), result[1].toString())
}

fun splitAlignment(text1 : String, text2 : String) : List<SplitAlignment>
{
    return splitAlignment(EnrichedString(text1), EnrichedString(text2))
}
package org.barend.babelmagic

private val LOG_TAG = "BARESIM"

// Calculate scores, sort responses and removes duplicate corrections
fun scoreResults(inputString : String, responses : List<TranslateResult>,
                 sort : Boolean = true, unique : Boolean = true) : List<TranslateResult>
{
    val result = mutableListOf<TranslateResult>()
    for (response in responses)
    {
        val aligned = alignWithSort(EnrichedString(inputString), EnrichedString(response.backText))
        val marked = alignMarkWords(aligned[0], aligned[1])
        val optimized = optimizeAlignment(marked, aligned[1])
        myLog(LOG_TAG, "Aligned 0: ${aligned[0]}")
        myLog(LOG_TAG, "Aligned 1: ${aligned[1]}")
        myLog(LOG_TAG, "Marked: ${marked.debugString()}")
        myLog(LOG_TAG, "Optimized 0: ${optimized[0]}")
        myLog(LOG_TAG, "Optimized 1: ${optimized[1]}")
        val score = getStringSimilarityScore(bareString(optimized[0]), bareString(optimized[1]))
        result.add(TranslateResult(response.viaLanguage, response.viaText, response.backText, score,
            alignedInput = optimized[0],
            alignedResponse = optimized[1]))
    }
    if (sort)
    {
        result.sort()
    }

    if (unique)
    {
        val toRemove = mutableSetOf<String>()

        for ((index, response) in responses.withIndex())
        {
            val bareResponse = bareString(response.backText)
            for (i in 0 until index)
            {
                if (bareResponse == bareString(responses[i].backText))
                {
                    toRemove.add(response.viaLanguage)
                }
            }
        }
        result.removeIf { x: TranslateResult -> toRemove.contains(x.viaLanguage) }
    }

    return result
}

// Build a string of the corrections for the purpose of Text To Speech.
fun stringifyCorrections(results : List<TranslateResult>) : String
{
    var result = ""
    for (r in results)
    {
        result += r.backText
        if (!result.endsWith('.'))
        {
            result += "."
        }
        result += "\n"
    }
    return result
}

fun resultsAsScoreArray(results : List<TranslateResult>) : Array<Double>
{
    return Array(results.size, { i -> results[i].bareScore})
}

fun isImproved(newResults : List<TranslateResult>, oldScores : Array<Double>) : Boolean
{
    if (oldScores.size != newResults.size)
    {
        return false
    }
    for ((index, oldScore) in oldScores.withIndex())
    {
        if (newResults[index].bareScore >= oldScore)
        {
            return true
        }
    }
    return false
}


// For the purpose of debugging
fun scoreString(scores : Array<Double>) : String
{
    var result = ""
    for (score in scores)
    {
        if (result.isNotEmpty())
        {
            result += " "
        }
        result += score.toString()
    }
    return result
}

fun isQuiteRight(scores : List<TranslateResult>) = scores.isNotEmpty() && scores[0].bareScore > 0.95

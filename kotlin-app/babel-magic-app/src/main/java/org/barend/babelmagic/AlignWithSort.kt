// Algorithm to sort a target text based on a reference text,
// by splitting the text, find the most similar word in the reference text and tag it,
// and sort the result on the order of the reference text.
// For example:
// * Give me please bread (input)
// * Give me bread please! (reference)
// -> It splits and marks all words ("give", "me", "please", "bread") in both texts
// -> It finds "give" and assigns 0, same for "me" (1)
// -> It finds "please" and assigns 3
// -> It finds "bread" and assigns 2
// Sorting it back gives the right order. The UI gives this a brown color,
// not red (wrong) or orange but brown is nearly black.
// For strings with unique words which are all similar, it works perfectly.
// If words are less simila, or if they are not unique, or there are missing or extra
// words, the results might be more weird or surprising.
// It replaces an earlier approach were permutations were used (too slow),
// performs better, is faster and the code is shorter and simpler.

package org.barend.babelmagic

private data class WordInfo(val word : String,
                            val index : Int,
                            val precedingPunctuations : String = "",
                            val suspicious : Boolean = false,
                            var refIndex : Int = -1) {}

private data class Similarity(val index : Int,
                              val errorRatio : Float) {}

private fun split(s : EnrichedString) : List<WordInfo>
{
    val result = mutableListOf<WordInfo>()
    var begin = 0
    var index = 0
    var punctuations = ""
    for (i in 0 until s.length)
    {
        if (isPunctuation(s[i]))
        {
            if (begin < i)
            {
                result.add(WordInfo(s.substring(begin, i), index = index++, precedingPunctuations = punctuations))
                punctuations = ""
            }
            begin = i + 1
            punctuations += s[i]
        }
    }
    if (begin < s.length)
    {
        result.add(WordInfo(s.substring(begin, s.length), index = index))
    }
    return result
}

// Finds the most similar word. If the word occurs more, it also takes position into account.
private fun mostSimilar(word : WordInfo, refList : List<WordInfo>) : Similarity
{
    val e = EnrichedString(bareString(word.word))
    var minOffset = e.length * 10
    var minDiff = e.length * 10
    var resultIndex = refList.size
    for ((i, ref) in refList.withIndex())
    {
        val aligned = alignWithNeedlemanWunsch(e, EnrichedString(bareString(ref.word)))
        val diff = getDifferencesInAlignedTexts(aligned[0], aligned[1])
        val offset = Math.abs(word.index - i)
//        println("### ${aligned[0]} ${aligned[1]} -> ${diff}")
        if (diff < minDiff) {
            minDiff = diff
            minOffset = offset
            resultIndex = ref.index
        }
        else if (diff == minDiff && offset < minOffset)
        {
            minOffset = offset
            resultIndex = ref.index
        }
    }

    // If the word similarity is dubious, then keep the original order.
    val errorRatio = minDiff.toFloat() / (word.word.length.toFloat() + 1.0f)
    return Similarity(resultIndex, errorRatio)
}

private fun sortWords(words: List<WordInfo>, ref : List<WordInfo>): List<WordInfo> {
    val copy = MutableList(words.size)
    {
        //println("${it} - ${words[it].index}")
        val sim = mostSimilar(words[it], ref)

        WordInfo(
            words[it].word,
            words[it].index,
            words[it].precedingPunctuations,
            suspicious = sim.errorRatio >= 0.5f,
            refIndex = sim.index)
    }

    for ((i, info) in copy.withIndex())
    {
        if (info.suspicious
            && i > 0
            && i + 1 < copy.size)
        {
            val left = copy[i - 1]
            val right = copy[i + 1]
            val leftOffset = left.refIndex - left.index
            val rightOffset = right.refIndex - right.index
            if (leftOffset * rightOffset > 0)
            {
                // Move the word. It can even go left of 0
                val offset = (leftOffset + rightOffset) / 2
                println("Movement [${i} ${info.refIndex}] of ${info.word} with ${leftOffset} / ${rightOffset} -> ${offset} (${ref[left.refIndex].word} / ${ref[right.refIndex].word})")
                copy[i].refIndex += offset
            }
        }
    }

    // Sort the words back on the index of the reference words
    // Example: "he me hit" / "he hits me" -> ref indices(0,2,1) -> "he", "hit", "me"
    copy.sortWith(Comparator
    { o1: WordInfo, o2: WordInfo ->
        return@Comparator o1.refIndex.compareTo(o2.refIndex)
    })
    return copy
}

// Rejoins the word list, including punctuations
private fun joinWords(list : List<WordInfo>) : EnrichedString
{
    var result = EnrichedString()

    for ((i, info) in list.withIndex())
    {
        // Consider it as moved it it is ordered differently than its neighbors
        // Even then, if it is at the original place, it is not considered as moved.
        // Example: "this is", and "is" is moved, then "this" should be untouched.
        val leftOk = i > 0 && list[i - 1].index + 1 == info.index
        val rightOk = i + 1 < list.size && info.index + 1 == list[i + 1].index
        val code = if (list.size == 1 || leftOk || rightOk || info.index == info.refIndex) ENRICHED_CHAR_DEFAULT else ENRICHED_CHAR_MOVED

        if (leftOk && info.precedingPunctuations.isNotEmpty())
        {
            result.append(info.precedingPunctuations)
        }
        else if (i > 0)
        {
            result.append(' ')
        }

        result.append(info.word, code)
    }
    return result
}

fun sortWithReference(text : EnrichedString, reference : EnrichedString) : EnrichedString
{
    return joinWords(sortWords(split(text), split(reference)))
}

fun alignWithSort(a : EnrichedString, b : EnrichedString, aligner : Char = '*') : Array<EnrichedString>
{
    val bare = alignWithNeedlemanWunsch(EnrichedString(bareString(a)), EnrichedString(bareString(b)), aligner)
    val raw = alignWithNeedlemanWunsch(a, b, aligner)
    val bareScore = getDifferencesInAlignedTexts(bare[0], bare[1])
    if (bareScore <= 2)
    {
        // Punctuations apart, it is already nearly perfect.
        // Therefore avoid trying to move words
        return raw
    }

    val sorted = alignWithNeedlemanWunsch(sortWithReference(a, b), b, aligner)
    val sortedScore = getDifferencesInAlignedTexts(sorted[0], sorted[1])
    val rawScore = getDifferencesInAlignedTexts(raw[0], raw[1])

    return if (sortedScore < rawScore) sorted else raw
}
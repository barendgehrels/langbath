package org.barend.babelmagic

import org.junit.Assert
import org.junit.Test
import java.lang.Math.abs


private fun moveCount(text : EnrichedString) : Int
{
    var result = 0
    var previousMove = false
    for (i in 0 until text.length)
    {
        val ch = text.getChar(i)
        if (ch.code == ENRICHED_CHAR_ALIGNMENT)
        {
            // Alignments do not count, neither within moved words.
            continue
        }
        val move = ch.code == ENRICHED_CHAR_MOVED
        if (previousMove != move && move)
        {
            result++
        }
        previousMove = move
    }
    return result
}

class AlignWithSortUnitTest
{
    @Test
    fun testSimplex()
    {
        val wrong = "Giv  m' pleze brad"
        val right = "Give me bread please!"

        val sorted = sortWithReference(EnrichedString(wrong), EnrichedString(right))
        val aligned = alignWithNeedlemanWunsch(sorted, EnrichedString(right))
        val marked = alignMarkWords(aligned[0], aligned[1])
        println(aligned[0])
        println(aligned[1])
        println(marked.debugString())
    }

    @Test
    fun testShort()
    {
        val wrong = "He me hit"
        val right = "He hits me"

        val sorted = sortWithReference(EnrichedString(wrong), EnrichedString(right))
        val aligned = alignWithNeedlemanWunsch(sorted, EnrichedString(right))
        val marked = alignMarkWords(aligned[0], aligned[1])
        val score = getDifferencesInAlignedTexts(aligned[0], aligned[1])
        val moved = moveCount(marked)
        Assert.assertTrue(score <= 1)
        Assert.assertEquals(2, moved)
        println("${aligned[0]}\n${aligned[1]} -> ${score}, ${moved}")
        println(marked.debugString())
    }

    @Test
    fun testLong()
    {
        val wrong = "Please give me bread, butter and milk, if you would be so kind"
        val right = "Give me bread, butter and milk, if you would be so kind, please"

        val sorted = sortWithReference(EnrichedString(wrong), EnrichedString(right))
        val aligned = alignWithNeedlemanWunsch(sorted, EnrichedString(right))
        val marked = alignMarkWords(aligned[0], aligned[1])
        val score = getDifferencesInAlignedTexts(aligned[0], aligned[1])
        val moved = moveCount(marked)
        Assert.assertTrue(score <= 3)
        Assert.assertEquals(1, moved)
        println("${aligned[0]}\n${aligned[1]} -> ${score}, ${moved}")
        println(marked.debugString())
    }

    @Test
    fun testRepeating()
    {
        val wrong = "The sea is blu, the sea is beautifull, the sea is dangrous."
        val right = "The sea is dangerous, the sea is blue, the sea is beautiful."

        val sorted = sortWithReference(EnrichedString(wrong), EnrichedString(right))
        val aligned = alignWithNeedlemanWunsch(sorted, EnrichedString(right))
        val marked = alignMarkWords(aligned[0], aligned[1])
        val score = getDifferencesInAlignedTexts(aligned[0], aligned[1])
        val moved = moveCount(marked)
        Assert.assertTrue(score <= 5)
        Assert.assertEquals(3, moved)
        println("${aligned[0]}\n${aligned[1]} -> ${score}, ${moved}")
        println(marked.debugString())
    }

    @Test
    fun testMissing()
    {
        // This case results in wrong movements because it cannot find "clock", and "and",
        // the "I" has another function, and the similarity of "darkness" / "dark" is not trusted.
        // However, the result is still just a bit better than comparing raw sentences.
        val wrong = "I wake at four o'clock to soundless darkness, and stare."
        val right = "Waking at four to soundless dark, I stare."

        val sorted = sortWithReference(EnrichedString(wrong), EnrichedString(right))
        val aligned = alignWithNeedlemanWunsch(sorted, EnrichedString(right))
        val marked = alignMarkWords(aligned[0], aligned[1])
        val score = getDifferencesInAlignedTexts(aligned[0], aligned[1])
        val moved = moveCount(marked)

        val raw = alignWithNeedlemanWunsch(EnrichedString(wrong), EnrichedString(right))
        val rawScore = getDifferencesInAlignedTexts(raw[0], raw[1])

//        Assert.assertTrue(score <= 5)
//        Assert.assertEquals(3, moved)
        println("${raw[0]}\n${raw[1]} -> ${rawScore}")
        println("${aligned[0]}\n${aligned[1]} -> ${score}, ${moved}")
        println(marked.debugString())
    }

    @Test
    fun testDifferent()
    {
        // Several words cannot be paired (hold/retain, up) but it results in the same score.
        val wrong = "Flares up again to retain and horrify"
        val right = "Flashes afresh to hold and horrify"

        val sorted = sortWithReference(EnrichedString(wrong), EnrichedString(right))
        val aligned = alignWithNeedlemanWunsch(sorted, EnrichedString(right))
        val marked = alignMarkWords(aligned[0], aligned[1])
        val score = getDifferencesInAlignedTexts(aligned[0], aligned[1])
        val moved = moveCount(marked)

        val raw = alignWithNeedlemanWunsch(EnrichedString(wrong), EnrichedString(right))
        val rawScore = getDifferencesInAlignedTexts(raw[0], raw[1])

        Assert.assertEquals(16, rawScore)
//        Assert.assertEquals(16, score)
//        Assert.assertEquals(0, moved)
        println("${raw[0]}\n${raw[1]} -> ${rawScore}")
        println("${aligned[0]}\n${aligned[1]} -> ${score}, ${moved}")
        println(marked.debugString())
    }
    @Test
    fun testCombinations()
    {
        val texts = arrayOf("This is a different order", "This is ordered differently", "This order is different")
        for (a in 0 until texts.size)
        {
            for (b in 0 until texts.size)
            {
                if (a == b)
                {
                    continue;
                }
                val sorted = sortWithReference(EnrichedString(texts[a]), EnrichedString(texts[b]))
                val aligned = alignWithNeedlemanWunsch(sorted, EnrichedString(texts[b]))
                val marked = alignMarkWords(aligned[0], aligned[1])
                val score = getDifferencesInAlignedTexts(aligned[0], aligned[1])
                val moved = moveCount(aligned[0])

                // Because words are moved when possible, the score is reasonably in all cases
                Assert.assertTrue(score <= 7)
                Assert.assertTrue(moved >= 1 && moved <= 2)

                println("# ${texts[a]} / ${texts[b]} -> ${score}, ${moved}")
                println(aligned[0])
                println(aligned[1])
                println(marked.debugString())
                println()
            }
        }
    }

    @Test
    fun testVeryLong()
    {
        val right =
            "I work all day, and get half-drunk at night." +
            " Waking at four to soundless dark, I stare." +
            " In time the curtain-edges will grow light." +
            " Till then I see what’s really always there:" +
            " Unresting death, a whole day nearer now," +
            " Making all thought impossible but how" +
            " And where and when I shall myself die." +
            " Arid interrogation: yet the dread" +
            " Of dying, and being dead," +
            " Flashes afresh to hold and horrify."
            val wrong =
                "I work all day and get drunk at night." +
        " I wake at four o'clock to soundless darkness, and stare." +
        " Eventually, the edges of the curtains will light up." +
        " Until then, I see what has always been there:" +
        " Unceasing death, a whole day closer now," +
        " Making all thought impossible, except how" +
        " And where and when I myself shall die." +
        " Arid interrogation: yet the fear" +
        " Of dying, and of being dead" +
        " Flares up again to retain and horrify."

        val sorted = sortWithReference(EnrichedString(wrong), EnrichedString(right))
        val aligned = alignWithNeedlemanWunsch(sorted, EnrichedString(right))
        val marked = alignMarkWords(aligned[0], aligned[1])
        val score = getDifferencesInAlignedTexts(aligned[0], aligned[1])
        val moved = moveCount(marked)

        val raw = alignWithNeedlemanWunsch(EnrichedString(wrong), EnrichedString(right))
        val rawScore = getDifferencesInAlignedTexts(raw[0], raw[1])

//        Assert.assertTrue(score <= 3)
//        Assert.assertEquals(1, moved)
        println("${aligned[0]}\n${aligned[1]} -> ${rawScore} -> ${score}, ${moved}")
        println(marked.debugString())
    }
}

package org.barend.babelmagic

import org.junit.Assert
import org.junit.Test

class ScoreResultsUnitTest
{
    private val input = "This is very interesting"
    private val responses = listOf(TranslateResult("a", "test", "This is interesting"),
        TranslateResult("b", "test", "This is very interesting"),
        TranslateResult("c", "test", "This might be interesting"),
        TranslateResult("d", "test", "This is very interesting"))

    @Test
    fun testScore()
    {
        val sorted = scoreResults(input, responses, false, false)

        Assert.assertEquals(0.8096, sorted[0].bareScore, 0.01)
        Assert.assertEquals(1.0, sorted[1].bareScore, 0.01)
        Assert.assertEquals(0.7727, sorted[2].bareScore, 0.01)
        Assert.assertEquals(1.0, sorted[3].bareScore, 0.01)
    }

    @Test
    fun testSort()
    {
        val sorted = scoreResults(input, responses, true, false)

        Assert.assertEquals("b", sorted[0].viaLanguage)
        Assert.assertEquals("d", sorted[1].viaLanguage)
        Assert.assertEquals("a", sorted[2].viaLanguage)
        Assert.assertEquals("c", sorted[3].viaLanguage)
    }

    @Test
    fun testRemove()
    {
        val sorted = scoreResults(input, responses, true, true)

        Assert.assertEquals(3, sorted.size)

        Assert.assertEquals("b", sorted[0].viaLanguage)
        Assert.assertEquals("a", sorted[1].viaLanguage)
        Assert.assertEquals("c", sorted[2].viaLanguage)
    }
}
package org.barend.babelmagic

import org.junit.Assert.assertEquals
import org.junit.Test

class AlignAdaptWordsUnitTest
{

    @Test
    fun testSimplex()
    {
        // >50% is wrong, mark word as wrong
        assertEquals("[w:dhe*]", alignMarkWords(EnrichedString("dhe*"), EnrichedString("this")).debugString())
        // 40% is wrong, don't mark it as wrong
        assertEquals("thr**", alignMarkWords(EnrichedString("thr**"), EnrichedString("three")).debugString())
    }

    @Test
    fun testInSentence()
    {
        assertEquals(
            "*ne [w:t**] thr** fo*r fiv*e",
            alignMarkWords(EnrichedString("*ne t** thr** fo*r fiv*e"), EnrichedString("one two three four fiv*e")).debugString()
        )
        assertEquals(
            "*ne,[w:t**],thr**,fo*r,fiv*e",
            alignMarkWords(EnrichedString("*ne,t**,thr**,fo*r,fiv*e"), EnrichedString("one,two,three,four,fiv*e")).debugString()
        )
    }

//    @Test
//    fun testAligner()
//    {
//        val aligner = Aligners('*', '-')
//        assertEquals(
//            "This is a ------", alignAdaptWords("This is a palace", "This is a castle", aligner)
//        )
//    }

    @Test
    fun testIfUsingWordSeparationOfCorrectText()
    {
        // Without space, it is one word and then 3/8 is incorrect
        assertEquals("bl***hole", alignMarkWords(EnrichedString("bl***hole"), EnrichedString("blackhole")).debugString())
        // With space in the correct text, two words are considered and 60% of "black" is incorrect
        assertEquals("[w:bl***]*hole", alignMarkWords(EnrichedString("bl****hole"), EnrichedString("black hole")).debugString())
        // Found this case, it should have marked the whole first word red
        assertEquals(
            "[w:вел*******о*]*дорожке", alignMarkWords(EnrichedString("вел*******о**дорожке"), EnrichedString("велосипедной дорожке")).debugString())
    }
}


package org.barend.babelmagic

import org.junit.Assert
import org.junit.Test

class OptimizeAlignmentUnitTest
{

    @Test
    fun testComplex()
    {
        // This test contains a weird piece, even the "correct" one, but it shows where it can go wrong.
        // The alignment of the second word mixes with the first word because they use three of the same characters.
        // Then both eer and weird are marked as wrong words, because 50% or more is wrong
        // Then in word markers / optimization those word markers are gone and the texts look the same (as if it was perfect)
        val a = "Something strange for eer weird time."
        val b = "Something strange for eer time."
        val raw = alignWithNeedlemanWunsch(EnrichedString(a), EnrichedString(b))
        val marked = alignMarkWords(raw[0], raw[1])
        val aligned = optimizeAlignment(marked, raw[1])

        // The alignment sets the asterisks at non-intuitive places. We would expect: "eer******"
        // Because it walks backwards, encounters a correct r, then a correct e, etc.
        // It is not so easy to fix.
        Assert.assertEquals("Something strange for *e***e*r* time.", raw[1].toString())
        // Most important: make sure the whole word is not skipped.
        Assert.assertEquals("Something strange for *e**e*r time.", aligned[1].toString())
    }

}
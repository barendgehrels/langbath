package org.barend.babelmagic

import org.junit.Assert.assertEquals
import org.junit.Test

class AlignWithEnrichedStringUnitTest
{
    @Test
    fun testSimplex()
    {
        var s1 = EnrichedString()
        s1.append("one")
        s1.append(' ')
        s1.append("tow")
        s1.append(' ')
        s1.append("thrie", ENRICHED_CHAR_MOVED)
        s1.append(' ')
        s1.append("for")
        s1.append(' ')
        s1.append("fivve")
        s1.append(' ')
        s1.append("six")
        println("s1 is ${s1} (${s1.debugString()})")

        // Verify if the strings survive the various stages
        var s2 = EnrichedString("one two three four five")
        var raw = alignWithNeedlemanWunsch(s1, s2)
        var word = alignMarkWords(raw[0], raw[1])
        val opt = optimizeAlignment(word, raw[1])

        println("""Results 
    ${raw[0]} (${raw[0].debugString()})
    ${raw[1]} (${raw[1].debugString()})
    ${word} (${word.debugString()})
    ${opt[0]} (${opt[0].debugString()})
    ${opt[1]} (${opt[1].debugString()})
    """)

        // One assertion is enough to check if the debug string still contains the code
        // for word order, and also for scratched and moved
        assertEquals("one t*ow [m:thrie] fo*r fivve [e:six]", opt[0].debugString())

    }

}


package org.barend.babelmagic

import org.junit.Assert
import org.junit.Assert.assertEquals
import org.junit.Test

class TranslateResultUnitTest
{
    @Test
    fun test()
    {
        var responses = arrayOf(TranslateResult("a", "test", "test", 0.9),
            TranslateResult("b", "test", "test", 0.7),
            TranslateResult("c", "test", "test", 1.0),
            TranslateResult("d", "test", "test", 0.8))

        responses.sort()

        Assert.assertEquals("c", responses[0].viaLanguage)
        Assert.assertEquals("a", responses[1].viaLanguage)
        Assert.assertEquals("d", responses[2].viaLanguage)
        Assert.assertEquals("b", responses[3].viaLanguage)
    }
}
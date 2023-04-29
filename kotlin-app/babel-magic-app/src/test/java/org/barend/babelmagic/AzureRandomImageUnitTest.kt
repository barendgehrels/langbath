package org.barend.babelmagic

import org.junit.Assert.*
import org.junit.Test

class AzureRandomImageUnitTest
{
    @Test
    fun test()
    {
        val info = createServiceCallInfo()
        if (!info.enabled)
        {
            return;
        }
        val answer = callRandomImage(info, RandomImageParameters("cat"))
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        assertFalse(msg, adapted.contains("Exception"))
        assertTrue(msg, adapted.contains(",id:"))
        assertTrue(msg, adapted.contains(",description:"))
        assertTrue(msg, adapted.contains(",url:"))
    }

    @Test
    fun testCoded()
    {
        val info = createServiceCallInfo(encrypt = true)
        if (!info.enabled)
        {
            return;
        }
        val answer = callRandomImage(info,
            RandomImageParameters("say \"yes\"")
        )
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        assertFalse(msg, adapted.contains("Exception"))
        assertTrue(msg, adapted.contains(",id:"))
        assertTrue(msg, adapted.contains(",description:"))
        assertTrue(msg, adapted.contains(",url:"))
    }
}
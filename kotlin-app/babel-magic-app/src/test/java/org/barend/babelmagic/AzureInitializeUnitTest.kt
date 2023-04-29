package org.barend.babelmagic

import org.junit.Assert
import org.junit.Test

private const val AZURE_ENTRY_INITIALIZE_FUNCTION_KEY = "azure.service.initialize.function.key"

class AzureInitializeUnitTest
{
    fun prepareCall(timeOffset : Int = 0) : ServiceCallInfo
    {
        if (!AZURE_UNIT_TESTS_ENABLED)
        {
            return ServiceCallInfo(enabled = false)
        }

        val map = testReadProperties()
        if (map.containsKey(AZURE_ENTRY_API) && map.containsKey(AZURE_ENTRY_KEY) && map.containsKey(AZURE_ENTRY_INITIALIZE_FUNCTION_KEY))
        {
            val inputKey = if (map.containsKey(AZURE_ENTRY_TEXT_KEY)) map[AZURE_ENTRY_TEXT_KEY]!! else ""
            return ServiceCallInfo(map[AZURE_ENTRY_API]!!, map[AZURE_ENTRY_INITIALIZE_FUNCTION_KEY]!!, map[AZURE_ENTRY_KEY]!!,
                textKey = inputKey,
                timeOffset = timeOffset)
        }
        return ServiceCallInfo(enabled = false)
    }

    @Test
    fun test()
    {
        val info = prepareCall()
        if (!info.enabled)
        {
            return
        }

        val answer = callInitialize(info, "")
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertFalse(msg, adapted.contains("Exception"))
        Assert.assertTrue(msg, adapted.contains(",userid:"))
        Assert.assertTrue(msg, adapted.contains(",fk_tts:"))
        Assert.assertTrue(msg, adapted.contains(",fk_ri:"))
        Assert.assertTrue(msg, adapted.contains(",fk_dl:"))
        Assert.assertTrue(msg, adapted.contains(",fk_tv:"))
    }

    @Test
    fun testSameUserId()
    {
        val info = prepareCall()
        if (!info.enabled)
        {
            return
        }

        val answer = callInitialize(info, userid = "12345")
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertFalse(msg, adapted.contains("Exception"))
        Assert.assertTrue(msg, adapted.contains(",userid:12345,"))
    }

    @Test
    fun testTimeOffset()
    {
        val info = prepareCall(timeOffset = 1000)
        if (!info.enabled)
        {
            return
        }

        val answer = callInitialize(info, "")
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertFalse(msg, adapted.contains("Exception"))
        Assert.assertTrue(msg, adapted.contains(",ts_diff:"))
    }
}
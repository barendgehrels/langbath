package org.barend.babelmagic

import org.junit.Assert
import org.junit.Test
import java.io.FileNotFoundException

class AzureTranslateViaUnitTest
{
    @Test
    fun test()
    {
        val info = createServiceCallInfo()
        if (!info.enabled)
        {
            return;
        }
        val answer = callTranslateVia(info, "de", "nl", "nl", "Ich sehe eine Katze.")
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, adapted.contains(",native:"))
        Assert.assertTrue(msg, adapted.contains("via:"))
        Assert.assertTrue(msg, adapted.contains(",back:"))
        Assert.assertTrue(msg, adapted.contains("code:"))
        Assert.assertTrue(msg, adapted.contains(",text:"))
        Assert.assertTrue(msg, adapted.contains(",engine:"))
    }

    @Test
    fun testErrorRequest()
    {
        val info = createServiceCallInfo()
        if (!info.enabled)
        {
            return;
        }
        val answer = callTranslateVia(info, "es", "nl", "", "")
        println(answer)
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, answer.contains("error"))
        Assert.assertTrue(msg, answer.contains("Pass parameters"))
    }

    @Test
    fun testErrorTime()
    {
        val info = createServiceCallInfo(timeOffset =  10000)
        if (!info.enabled)
        {
            return;
        }
        val answer = callTranslateVia(info, "es", "nl", "", "Alas, wrong timing")
        println(answer)
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, answer.contains("error"))
        Assert.assertTrue(msg, answer.contains("Time elapsed"))
    }

    @Test
    fun testErrorInEphemeralKey()
    {
        val info = createServiceCallInfo(ephemeralKey = "error")
        if (!info.enabled)
        {
            return;
        }
        val answer = callTranslateVia(info, "es", "nl", "", "Timestamp encryption error")
        println(answer)
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, answer.contains("error"))
        Assert.assertTrue(msg, answer.contains("[TV] Decoding not successful"))
    }

    @Test
    fun testErrorInTextKey()
    {
        val info = createServiceCallInfo(encrypt = true, textKey = "wrong")
        if (!info.enabled)
        {
            return;
        }
        val answer = callTranslateVia(info, "es", "nl", "", "Alas, wrong key")
        println(answer)
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, answer.contains("error"))
        Assert.assertTrue(msg, answer.contains("Decoding"))
        Assert.assertTrue(msg, answer.contains("not successful"))
    }

    @Test
    fun testErrorInFunctionKey()
    {
        val info = createServiceCallInfo(functionKey = "wrong")
        if (!info.enabled)
        {
            return;
        }
        try
        {
            callTranslateVia(info, "es", "nl", "", "Wrong function key/code")
        }
        catch(ex : Exception)
        {
            // This is as expected
            println("OK: service is not found ${ex.message}")
            return
        }
        Assert.assertTrue("ERROR, service is found", false)
    }

    @Test
    fun testErrorLanguageCode()
    {
        val info = createServiceCallInfo()
        if (!info.enabled)
        {
            return;
        }
        val answer = callTranslateVia(info, "XX", "nl", "", "Es kann nicht gut sein.")
        println(answer)
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, answer.contains("error"))
        Assert.assertTrue(msg, answer.contains("Language"))
        Assert.assertTrue(msg, answer.contains("not supported"))
    }

    @Test
    fun testUnsupportedNativeLanguageCode()
    {
        val info = createServiceCallInfo()
        if (!info.enabled)
        {
            return;
        }
        val answer = callTranslateVia(info, "en", "es", "ga", "The gaelic native language is not problematic.")
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        // The native tag is there, but its content is empty.
        Assert.assertTrue(msg, adapted.contains(",native:code:null"))
        Assert.assertTrue(msg, adapted.contains(",via:code:es"))
    }

    @Test
    fun testTooShort()
    {
        val info = createServiceCallInfo()
        if (!info.enabled)
        {
            return;
        }
        val answer = callTranslateVia(info, "de", "nl", "", "Hello.")
        println(answer)
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, answer.contains("error"))
        Assert.assertTrue(msg, answer.contains("too short"))
    }

    @Test
    fun testTooLong()
    {
        val info = createServiceCallInfo()
        if (!info.enabled)
        {
            return;
        }

        var s = ""
        while (s.length < INPUT_TEXT_LENGTH_MAX)
        {
            s += "Hello world. "
        }

        val answer = callTranslateVia(info, "en", "nl", "", s)
        println(answer)
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, answer.contains("error"))
        Assert.assertTrue(msg, answer.contains("too long"))
    }

    @Test
    fun testEncryption()
    {
        val info = createServiceCallInfo(encrypt = true)
        if (!info.enabled)
        {
            return;
        }

        val answer = callTranslateVia(info, "nl", "de", "", "Deze tekst wordt gecodeerd verstuurd en ontvangen.")
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, adapted.contains(",via:code:de"))
        // The == indicates the base64 encoded text
        Assert.assertTrue(msg, adapted.contains("==,engine:mock"))
    }

    @Test
    fun testEncryptionNotMocked()
    {
        val info = createServiceCallInfo(encrypt = true, mock = false)
        if (!info.enabled)
        {
            return;
        }

        val answer = callTranslateVia(info, "nl", "de", "", "Deze tekst wordt gecodeerd verstuurd.")
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, adapted.contains(",via:code:de"))
        Assert.assertTrue(msg, adapted.contains("verschlüsselt"))
    }

}

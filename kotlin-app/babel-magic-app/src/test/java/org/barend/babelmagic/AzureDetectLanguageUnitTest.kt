package org.barend.babelmagic

import org.junit.Assert
import org.junit.Test

class AzureDetectLanguageUnitTest
{
    // Input: a string like ",supported:true,code:es,via:[fr,it],score:0.42,text:Hello world,"
    // Output: "Hello world"
    private fun getText(adapted : String) : String
    {
        val tag = ",text:"
        val pos = adapted.indexOf(tag)
        if (pos == -1) { return "" }
        val outputText = adapted.substring(pos + tag.length)
        return outputText.substring(0, outputText.length - 1)
    }

    // Input: a string like ",supported:true,code:es,via:[fr,it],score:0.42,text:NR7Dya+1w==,
    // Output: "Hi!", or whatever is the decrypted text
    private fun getDecryptedText(adapted : String, info : ServiceCallInfo) : String
    {
        return decryptedOutput(getText(adapted), info.textKey, info.versionCode)
    }

    @Test
    fun test()
    {
        val info = createServiceCallInfo()
        if (!info.enabled)
        {
            return;
        }
        val answer = callDetectLanguage(info, "nl", "Veo un gato y el gato está sentado en el suelo.")
        val adapted = adaptJson(answer)
        val text = getText(adapted)
        println("Answer: ${answer}\nAdapted: ${adapted}\nText: ${text}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, adapted.contains(",supported:true,"))
        Assert.assertTrue(msg, adapted.contains(",code:es,"))
        Assert.assertTrue(msg, adapted.contains(",via:["))
        Assert.assertTrue(msg, adapted.contains(",score:"))
        Assert.assertTrue(msg, adapted.contains(",text:"))
        // The language detection service is mocked and always returns an adapted version of the original text
        Assert.assertEquals("Hi!(Veoungatoyelgatoestásentadoenelsuelo.)", text)
    }

    @Test
    fun testErrorRequest()
    {
        val info = createServiceCallInfo()
        if (!info.enabled)
        {
            return;
        }
        val answer = callDetectLanguage(info, "nl", "")
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, answer.contains("error"))
        Assert.assertTrue(msg, answer.contains("Pass parameters"))
    }

    @Test
    fun testUnsupportedLanguage()
    {
        val info = createServiceCallInfo(mock = false)
        if (!info.enabled)
        {
            return;
        }
        val answer = callDetectLanguage(info, "en", "Is ceann de na teangacha Ceilteacha í an Ghaeilge.")
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, adapted.contains(",supported:false,"))
        Assert.assertTrue(msg, adapted.contains(",code:ga,"))
        Assert.assertTrue(msg, adapted.contains(",via:[],"))
        Assert.assertTrue(msg, adapted.contains(",score:"))
        Assert.assertTrue(msg, adapted.contains(",text:"))
    }

    @Test
    fun testUnsupportedNativeLanguage()
    {
        val info = createServiceCallInfo(mock = false)
        if (!info.enabled)
        {
            return;
        }
        // Call it with an unsupported native language, but the learning language is English
        // This should be supported for detection and translation.
        val answer = callDetectLanguage(info, "ga", "This is not Gaelic.")
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, adapted.contains(",supported:true,"))
        Assert.assertTrue(msg, adapted.contains(",code:en,"))
        Assert.assertTrue(msg, adapted.contains("Gàidhlig"))
    }

    @Test
    fun testWrongNativeLanguage()
    {
        val info = createServiceCallInfo(mock = false)
        if (!info.enabled)
        {
            return;
        }
        // Call it with an invalid native language.
        // The native language is not important for the detection itself, but the Azure service
        // uses it to also return the translation in the native language.
        val answer = callDetectLanguage(info, "XX", "This won't work.")
        val adapted = adaptJson(answer)
        println("Answer: ${answer}\nAdapted: ${adapted}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, answer.contains("error"))
        // This is the answer from Microsoft Translate service, propagated to the caller.
        Assert.assertTrue(msg, answer.contains("The target language is not valid"))
    }

    // Sends a text encrypted to the server and decrypts it again.
    // Because the service is mocked, it starts with Hi! and then returns the text itself.
    @Test
    fun testWithoutEncryption()
    {
        val info = createServiceCallInfo(encrypt = false)
        if (!info.enabled)
        {
            return;
        }

        val answer = callDetectLanguage(info, "de", "Este texto no se envía codificado.")
        val adapted = adaptJson(answer)
        val text = getText(adapted)
        println("Answer: ${answer}\nAdapted: ${adapted}\nText: ${text}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, adapted.contains(",code:es"))
        Assert.assertTrue(msg, text.contains("Hi!(Estetextonose"))
    }

    // Sends a text encrypted to the server and decrypts it again.
    // Because the service is mocked, it starts with Hi! and then returns the text itself.
    @Test
    fun testEncryption()
    {
        val info = createServiceCallInfo(encrypt = true)
        if (!info.enabled)
        {
            return;
        }

        val answer = callDetectLanguage(info, "de", "Este texto se envía codificado al servidor.")
        val adapted = adaptJson(answer)
        val text = getDecryptedText(adapted, info)
        println("Answer: ${answer}\nAdapted: ${adapted}\nText: ${text}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, adapted.contains(",code:es"))
        Assert.assertTrue(msg, text.contains("Hi! (Este texto"))
    }

    @Test
    fun testQuoted()
    {
        val info = createServiceCallInfo(mock = false, encrypt = true)
        if (!info.enabled)
        {
            return;
        }

        val answer = callDetectLanguage(info, "de", "And he said: \"Just do it!\".")
        val adapted = adaptJson(answer)
        val text = getDecryptedText(adapted, info)
        println("Answer: ${answer}\nAdapted: ${adapted}\nText: ${text}")
        val msg = "Wrong answer: ${answer}"
        Assert.assertTrue(msg, adapted.contains(",code:en"))
        Assert.assertTrue(msg, text.contains("Und er sagte:"))
    }

    @Test
    fun testEncryptedInputShowInput()
    {
        // This is not a real unit test, but to show input which can be used
        // by testing the Azure service in the Azure environment itself.
        val info = createServiceCallInfo()
        println(encryptedInput("кошка сидит на полу", info.textKey))
    }
}
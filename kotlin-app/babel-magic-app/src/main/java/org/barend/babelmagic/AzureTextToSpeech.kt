// Calls the Azure TextToSpeech service

package org.barend.babelmagic

import android.os.Handler
import android.os.Looper
import org.json.JSONObject
import java.net.URL
import java.util.*
import java.util.concurrent.Executors

fun callTextToSpeech(sci : ServiceCallInfo, langCode : String, text : String) : String
{
    val json = """
            { 
              "version" : ${sci.versionCode},
              "timestamp" : "${sci.GetTimestamp()}",
              "lang" : "${langCode}",
              "text" : "${sci.GetInputText(text)}"
            }
            """
    return retrieveText(URL(sci.GetUrl(ACTION_TEXT_TO_SPEECH)), json.trimIndent())
}

private fun textToSpeech(sci : ServiceCallInfo,
                         langCode : String,
                         text : String,
                         processResult : (bytes : ByteArray) -> Unit,
                         processError : (errorMessage : String) -> Unit)
{
    var jsonString = ""
    try
    {
        jsonString = callTextToSpeech(sci, langCode, text)
    }
    catch (e : Exception)
    {
        Handler(Looper.getMainLooper()).post { processError(exceptionMessage(e, ACTION_TEXT_TO_SPEECH)) }
        return
    }

    try
    {
        val json = JSONObject(jsonString)
        val code = json.getString("code")
        val voice = json.getString("voice")
        val encodedBytes = json.getString("bytes")
        val decoder = Base64.getDecoder()
        val bytes = decoder.decode(encodedBytes)

        myLog(TAG_AZURE, "Speech retrieved: ${code}, ${voice}, ${bytes.size}")
        Handler(Looper.getMainLooper()).post { processResult(bytes) }
    }
    catch (e : Exception)
    {
        Handler(Looper.getMainLooper()).post { processError(exceptionJsonMessage(e, jsonString)) }
    }
}

fun getTextToSpeech(sci : ServiceCallInfo,
    langCode : String,
    text : String,
    processResult : (bytes : ByteArray) -> Unit,
    processError : (errorMessage : String) -> Unit
)
{
    Executors.newSingleThreadExecutor().execute {
        textToSpeech(
            sci, langCode, text, processResult, processError
        )
    }
}
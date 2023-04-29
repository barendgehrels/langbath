// Calls the Azure DetectLanguage service

package org.barend.babelmagic

import android.os.Handler
import android.os.Looper
import org.json.JSONObject
import java.net.URL
import java.util.concurrent.Executors

data class LanguageDetection(
    val languageCode : String = "",
    val score : Float = 0.0f,
    val supported : Boolean = false,
    val native : String = "",
    val via : Array<String> = arrayOf()
)
{
    fun getVia() : String
    {
        return via.joinToString(prefix = "[", postfix = "]")
    }
}

// Detect language of input text.
// The input text is encrypted, for the sake of providing a correct json (no need to double quote etc).
// and for the sake of some privacy for the user.
// Function callable from unit test
fun callDetectLanguage(sci : ServiceCallInfo, nativeLanguage : String, text : String) : String
{
    val json = """
            { 
              "version" : ${sci.versionCode},
              "timestamp" : "${sci.GetTimestamp()}",
              "native" : "${nativeLanguage}",
              "text" : "${sci.GetInputText(text)}"
              ${sci.GetMock()}
            }
            """
    return retrieveText(URL(sci.GetUrl(ACTION_DETECT_LANGUAGE)), json.trimIndent())
}

// Function calling detection, parsing the json and deliver the result in the main thread
// This function itself should NOT be called from the main thread.
private fun detectAndProcess(sci : ServiceCallInfo,
                             nativeLanguage : String,
                             text : String,
                             processResult : (detection : LanguageDetection) -> Unit,
                             processError : (errorMessage : String) -> Unit)
{
    var jsonString = ""

    try
    {
        jsonString = callDetectLanguage(sci, nativeLanguage, text)
    }
    catch (e : Exception)
    {
        Handler(Looper.getMainLooper()).post { processError(exceptionMessage(e, ACTION_DETECT_LANGUAGE)) }
        return
    }

    try
    {

        val json = JSONObject(jsonString)
        if (json.has("error"))
        {
            val message = json.getString("error")
            Handler(Looper.getMainLooper()).post { processError(message) }
            return
        }
        val via = json.getJSONArray("via")
        val text = decryptedOutput(json.getString("text"), sci.textKey, sci.versionCode)
        val detection = LanguageDetection(json.getString("code"),
            json.getDouble("score").toFloat(),
            json.getBoolean("supported"),
            text,
            Array<String>(via.length()) { via.getString(it) })

        Handler(Looper.getMainLooper()).post { processResult(detection) }
    }
    catch (e : Exception)
    {
        Handler(Looper.getMainLooper()).post { processError(exceptionJsonMessage(e, jsonString)) }
    }
}

// Main language detection function,
// calling detection in another thread, with a lambda to use the result in the main thread.
fun azureDetectLanguage(sci : ServiceCallInfo,
    nativeLanguage : String,
    text : String,
    processResult : (detection : LanguageDetection) -> Unit,
    processError : (errorMessage : String) -> Unit
)
{
    Executors.newSingleThreadExecutor().execute {
        detectAndProcess(
            sci, nativeLanguage, text, processResult, processError
        )
    }
}
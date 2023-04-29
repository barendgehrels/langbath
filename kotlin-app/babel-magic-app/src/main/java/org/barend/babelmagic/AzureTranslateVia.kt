// Calls the Azure TranslateVia service

package org.barend.babelmagic

import android.os.Handler
import android.os.Looper
import android.util.Log
import org.json.JSONObject
import java.net.URL
import java.util.concurrent.Executors

data class TranslateViaParameters(val language : String = "",
                                  val viaLanguage : String = "",
                                  val nativeLanguage : String = "",
                                  val text : String = "") {}

fun callTranslateVia(sci : ServiceCallInfo, language : String,
                     viaLanguage : String,
                     nativeLanguage : String,
                     text : String) : String
{
    val json = """
            { 
              "version" : ${sci.versionCode},
              "timestamp" : "${sci.GetTimestamp()}",
              "lang" : "${language}",
              "via" : "${viaLanguage}",
              "native" : "${nativeLanguage}",
              "text" : "${sci.GetInputText(text)}"
              ${sci.GetMock()}
            }
            """
    return retrieveText(URL(sci.GetUrl(ACTION_TRANSLATE_VIA)), json.trimIndent())
}

private fun translateVia(sci : ServiceCallInfo,
                         par : TranslateViaParameters,
                         processResult : (result : TranslateResult, native : String) -> Unit,
                         processError : (errorMessage : String) -> Unit)
{
    var nativeResult = ""

    var jsonString = ""

    try
    {
        jsonString = callTranslateVia(sci,
            language = par.language,
            viaLanguage = par.viaLanguage,
            nativeLanguage = if (nativeResult.isEmpty()) par.nativeLanguage else "",
            text = par.text)
    }
    catch (e : Exception)
    {
        Handler(Looper.getMainLooper()).post { processError(exceptionMessage(e, ACTION_TRANSLATE_VIA)) }
        return
    }

    Log.i(TAG_ENCRYPT, jsonString)

    try
    {
        val json = JSONObject(jsonString)
        if (json.has("error"))
        {
            val message = json.getString("error")
            Handler(Looper.getMainLooper()).post { processError(message) }
            return
        }
        if (nativeResult.isEmpty())
        {
            val nativeObj = json.getJSONObject("native")
            nativeResult = decryptedOutput(nativeObj.getString("text"), sci.textKey, sci.versionCode)
        }

        val viaObj = json.getJSONObject("via")
        val viaLanguage = viaObj.getString("code")
        val viaText = decryptedOutput(viaObj.getString("text"), sci.textKey, sci.versionCode)
        val viaEngine = viaObj.getString("engine")

        val backObj = json.getJSONObject("back")
        val backText = decryptedOutput(backObj.getString("text"), sci.textKey, sci.versionCode)
        val backEngine = backObj.getString("engine")

        val result = TranslateResult(
                viaLanguage,
                viaText,
                backText,
                0.0,
                viaEngine,
                backEngine)
        Handler(Looper.getMainLooper()).post { processResult(result, nativeResult) }
    }
    catch (e : Exception)
    {
        Handler(Looper.getMainLooper()).post { processError(exceptionJsonMessage(e, jsonString)) }
    }

}

fun verifyTextWithTranslateVia(sci : ServiceCallInfo,
    par : TranslateViaParameters,
    processResult : (result : TranslateResult, native : String) -> Unit,
    processError : (errorMessage : String) -> Unit)
{
    Executors.newSingleThreadExecutor().execute {
        translateVia(
            sci, par, processResult, processError
        )
    }
}
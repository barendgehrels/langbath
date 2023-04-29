// Calls the Azure RandomImage service to indirectly call Unsplash

package org.barend.babelmagic

import android.os.Handler
import android.os.Looper
import org.json.JSONObject
import java.net.URL
import java.util.concurrent.Executors

data class ImageInfo(val id : String, val description : String, val url : String, val link : String, val thumbnail : String) {}
data class RandomImageParameters(val query : String, val orientation : String = "") {}

fun callRandomImage(sci : ServiceCallInfo,
                    par : RandomImageParameters) : String
{
    val json = """
            { 
              "version" : ${sci.versionCode},
              "timestamp" : "${sci.GetTimestamp()}",
              "query" : "${sci.GetInputText(par.query)}",
              "orientation" : "${par.orientation}"
            }
            """
    return retrieveText(URL(sci.GetUrl(ACTION_RANDOM_IMAGE)), json.trimIndent())
}

private fun randomImage(sci : ServiceCallInfo,
                        par : RandomImageParameters,
                        processResult : (info : ImageInfo) -> Unit,
                        processError : (errorMessage : String) -> Unit)
{
    var jsonString = ""

    try
    {
        jsonString = callRandomImage(sci, par)
    }
    catch (e : Exception)
    {
        Handler(Looper.getMainLooper()).post { processError(exceptionMessage(e, ACTION_RANDOM_IMAGE)) }
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
        val info = ImageInfo(json.getString("id"),
            json.getString("description"),
            json.getString("url"),
            json.getString("link"),
            json.getString("thumb"))

        Handler(Looper.getMainLooper()).post { processResult(info) }
    }
    catch (e : Exception)
    {
        Handler(Looper.getMainLooper()).post { processError(exceptionJsonMessage(e, jsonString)) }
    }
}

fun getRandomImage(sci : ServiceCallInfo,
    par : RandomImageParameters,
    processResult : (info : ImageInfo) -> Unit,
    processError : (errorMessage : String) -> Unit
)
{
    Executors.newSingleThreadExecutor().execute {
        randomImage(
            sci, par, processResult, processError
        )
    }
}
// Calls the Azure Initialize service

package org.barend.babelmagic

import android.os.Handler
import android.os.Looper
import android.util.Log
import org.json.JSONObject
import java.net.URL
import java.util.concurrent.Executors

fun callInitialize(sci : ServiceCallInfo, userid : String) : String
{
    val json = """
            { 
              "version" : ${sci.versionCode},
              "timestamp" : "${sci.GetTimestamp()}",
              "userid" : "${userid}"
            }
            """
    return retrieveText(URL(sci.GetUrl(ACTION_INITIALIZE)), json.trimIndent())
}

private fun doInitialize(sci : ServiceCallInfo,
                       userid : String,
                       fkKey : String,
                       processResult : (info : AzureServiceInfo) -> Unit,
                       processError : (errorMessage : String) -> Unit)
{
    var jsonString = ""
    try
    {
        jsonString = callInitialize(sci, userid)
    }
    catch (e : Exception)
    {
        Handler(Looper.getMainLooper()).post { processError(exceptionMessage(e, ACTION_INITIALIZE)) }
        return
    }

    try
    {
        val encryption = AzureCompatibleAesEncryption()
        val json = JSONObject(jsonString)
        val info = AzureServiceInfo(userId = json.getString("userid"),
            tsDiff = json.getInt("ts_diff"),
            fkTts = encryption.decrypt(json.getString("fk_tts"), fkKey),
            fkRi = encryption.decrypt(json.getString("fk_ri"), fkKey),
            fkDl = encryption.decrypt(json.getString("fk_dl"), fkKey),
            fkTv = encryption.decrypt(json.getString("fk_tv"), fkKey))
        Log.i(TAG_AZURE, "User: ${info.userId} diff: ${info.tsDiff}")
        Handler(Looper.getMainLooper()).post { processResult(info) }
    }
    catch (e : Exception)
    {
        Handler(Looper.getMainLooper()).post { processError(exceptionJsonMessage(e, jsonString)) }
    }
}

fun Initialize(
    sci : ServiceCallInfo,
    userid : String,
    fkKey : String,
    processResult : (info : AzureServiceInfo) -> Unit,
    processError : (errorMessage : String) -> Unit
)
{
    Executors.newSingleThreadExecutor().execute {
        doInitialize(
            sci, userid, fkKey, processResult, processError
        )
    }
}
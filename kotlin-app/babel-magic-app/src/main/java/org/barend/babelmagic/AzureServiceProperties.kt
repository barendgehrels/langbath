package org.barend.babelmagic

const val ACTION_INITIALIZE = "Initialize"
const val ACTION_DETECT_LANGUAGE = "DetectLanguage"
const val ACTION_RANDOM_IMAGE = "RandomImage"
const val ACTION_TEXT_TO_SPEECH = "TextToSpeech"
const val ACTION_TRANSLATE_VIA = "TranslateVia"

// const val ACTION_TEXT_TO_SPEECH = "Beta"
// const val ACTION_TRANSLATE_VIA = "Beta"
// const val ACTION_DETECT_LANGUAGE = "Beta"
// const val ACTION_RANDOM_IMAGE = "Beta"

// For logging
const val TAG_AZURE = "AZURE"
const val TAG_SPEECH = "SPEECH"
const val TAG_ENCRYPT = "ENCRYPT"

// Constants shared with the server
const val INPUT_TEXT_LENGTH_MIN = 10
const val INPUT_TEXT_LENGTH_MAX = 10 * 42
const val SUBJECT_LENGTH_MAX = 24

// Structure returned by Initialize. It contains the userid (stored in the app settings),
// the time difference with the service, and Function Key's for the services
data class AzureServiceInfo(val userId : String = "",
                            val tsDiff : Int = 0,
                            val fkTts : String = "",
                            val fkRi : String = "",
                            val fkDl : String = "",
                            val fkTv : String = "") {}

// Structure to pack parameters for calls to Azure services
// The api is the same for all services
// The key is the ephemeral key and also the same for all services
// The textKey is to encode text (avoiding quotes etc) and used by DetectLanguage, TranslateVia and RandomImage
// The versionCode is from the app itself, unit tests might pass a magic version number to bypass encryption.
// The function key differs per service
data class ServiceCallInfo(val api : String = "",
                           val functionKey : String = "",
                           val key : String = "",
                           val textKey : String = "",
                           val versionCode : Int = BuildConfig.VERSION_CODE,
                           val timeOffset : Int = 0,
                           val mock : Boolean = false,
                           val enabled : Boolean = true)
{
    fun GetUrl(action : String) = "${api}/${action}?code=${functionKey}"
    fun GetMock() = if (mock) ", \"mock\" : true" else ""
    fun GetTimestamp() = encryptedTimestamp(key, timeOffset)
    fun GetInputText(text : String) = encryptedInput(text, textKey, versionCode)
}
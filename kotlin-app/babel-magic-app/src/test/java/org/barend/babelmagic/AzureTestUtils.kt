package org.barend.babelmagic

import java.io.File

// Only put it to "true" when you want to test if the Azure services are working as expected.
const val AZURE_UNIT_TESTS_ENABLED = false

const val AZURE_ENTRY_API = "azure.service.api.url"
const val AZURE_ENTRY_KEY = "azure.service.api.key"
const val AZURE_ENTRY_TEXT_KEY = "azure.service.text.key"
const val TAG_VERSION_CODE_WITHOUT_ENCRYPTION = "version.code.without.encryption"

const val AZURE_FUNCTION_KEY_UNIT_TEST = "Wu0nMi7tztAeWsqtGujnhiNtMtReWsctv"


data class TestCallInfo(val enabled : Boolean, val api : String = "", val key : String = "", val textKey : String = "") {}

// Function to read properties (local.properties),
// because unit tests are not (easily) able to access resources.
private fun testReadProperties(fileName : String) : Map<String, String>
{
    var result = mutableMapOf<String, String>()
    val file = File(fileName)
    file.forEachLine()
    {
        val s = it.trim()
        if (!s.isEmpty() && !s.startsWith("#") && s.contains('='))
        {
            val pos = s.indexOf('=')
            val name = s.substring(0, pos)
            val value = s.substring(pos + 1)
            result.put(name, value)
        }
    }
    return result
}

fun testReadProperties() : Map<String, String>
{
    return testReadProperties("../local.properties")
}

fun createServiceCallInfo(functionKey : String = AZURE_FUNCTION_KEY_UNIT_TEST,
                          versionCode : Int = BuildConfig.VERSION_CODE,
                          timeOffset : Int = 0,
                          mock : Boolean = true,
                          encrypt : Boolean = false,
                          ephemeralKey : String = "",
                          textKey : String = "") : ServiceCallInfo
{
    if (!AZURE_UNIT_TESTS_ENABLED)
    {
        return ServiceCallInfo(enabled = false)
    }

    val map = testReadProperties()
    if (map.containsKey(TAG_VERSION_CODE_WITHOUT_ENCRYPTION))
    {
        val code = map[TAG_VERSION_CODE_WITHOUT_ENCRYPTION]!!.toIntOrNull()
        VERSION_CODE_WITHOUT_ENCRYPTION = code ?: 0
    }
    if (map.containsKey(AZURE_ENTRY_API))
    {
        val ek = when
        {
            ephemeralKey.isNotEmpty() -> ephemeralKey
            map.containsKey(AZURE_ENTRY_KEY) -> map[AZURE_ENTRY_KEY]!!
            else -> ""
        }
        val tk = when
        {
            textKey.isNotEmpty() -> textKey
            map.containsKey(AZURE_ENTRY_TEXT_KEY) -> map[AZURE_ENTRY_TEXT_KEY]!!
            else -> ""
        }
        return ServiceCallInfo(api = map[AZURE_ENTRY_API]!!,
            functionKey = functionKey,
            key = ek,
            textKey = tk,
            versionCode = if (encrypt) versionCode else VERSION_CODE_WITHOUT_ENCRYPTION,
            timeOffset = timeOffset,
            mock = mock)
    }
    return ServiceCallInfo(enabled = false)
}

// Removes quotes, spaces, curly braces from json to test them more conveniently
fun adaptJson(s : String) : String
{
    return "," + s.replace("\"", "").replace(" ", "").replace("{", "").replace("}", "") + ","
}

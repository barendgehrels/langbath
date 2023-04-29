// Contains string or string related functionality

package org.barend.babelmagic

import android.util.Log
import java.io.FileNotFoundException
import java.text.Normalizer

private val REGEX_UNACCENT = "\\p{InCombiningDiacriticalMarks}+".toRegex()
private val ENABLE_ANDROID_LOGGING = false

fun myLog(tag : String, s : String)
{
    // Logging is just used for debugging in this app, don't log everything.
    if (ENABLE_ANDROID_LOGGING)
    {
        Log.v(tag, s);
    }
}

// Extent the "char" object with an unaccent method.
// Adapted from https://stackoverflow.com/questions/51731574/removing-accents-and-diacritics-in-kotlin
fun Char.unaccent() : Char
{
    val temp = Normalizer.normalize("" + this, Normalizer.Form.NFD)
    return REGEX_UNACCENT.replace(temp, "")[0]
}

fun bareString(s : String) : String
{
    var result = ""
    for (ch in s)
    {
        if (!isPunctuation(ch))
        {
            result += ch.unaccent().lowercase()
        }
    }
    return result
}

fun bareString(s : EnrichedString) : String
{
    var result = ""
    for (ch in s)
    {
        if (!isPunctuation(ch))
        {
            result += ch.unaccent().lowercase()
        }
    }
    return result
}

private fun subStringOf(s : String?, len : Int = 30) : String
{
    return when {
        s == null -> ""
        s.length > len -> s.substring(0, len)
        else -> s
    }
}

fun exceptionMessage(e : Exception, context : String) : String
{
    if (e is FileNotFoundException && e.message != null && e.message!!.startsWith("http") && e.message!!.contains("//"))
    {
        val c = if (context.isNotEmpty()) " for $context" else ""
        return "Service not found${c}"
    }
    return "Exception: ${e.message}"
}

fun exceptionJsonMessage(e : Exception, json : String) : String
{
    return "Exception: ${e.message} in ${subStringOf(json)}..."
}

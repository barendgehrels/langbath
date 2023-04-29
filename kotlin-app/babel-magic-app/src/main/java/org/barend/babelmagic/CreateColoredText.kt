// Functionality to create colored text
// It uses the Android "spannable string" functionality
// - errors are red
// - punctuations are orange (otherwise hardly visible)
// - moved texts are brown
// - difference in diacritics are green (this is usually fine)
// - differences in cases are blue (this is also fine)

package org.barend.babelmagic

import android.content.Context
import android.graphics.*
import android.text.SpannableString
import android.text.Spanned
import android.text.TextUtils
import android.text.style.ForegroundColorSpan
import androidx.core.content.ContextCompat

private fun getColor(context : Context, similarity : Int, altCode : Int = 0) : Int
{
    val altFactor = 0.4f
    return when (similarity)
    {
        // The higher, the grayer
        SIMILARITY_EQUAL -> Color.valueOf(altCode * altFactor, altCode * altFactor, altCode * altFactor).toArgb()
        SIMILARITY_PUNCTUATION -> ContextCompat.getColor(context, R.color.color_punctuation_difference)
        SIMILARITY_CASE -> ContextCompat.getColor(context, R.color.color_case_difference)
        SIMILARITY_ACCENT -> ContextCompat.getColor(context, R.color.color_accent_difference)
        SIMILARITY_DIFFERENT -> Color.RED
        SIMILARITY_MOVED -> ContextCompat.getColor(context, R.color.color_moved)
        else -> Color.MAGENTA
    }
}

fun createColoredText(
    context : Context, split : List<SplitAlignment>, altCode : Int = 0
) : CharSequence
{
    val resultSpan = SpannableString(splitAlignmentToString(split))

    var begin = 0
    for (s in split)
    {
        val end = begin + s.text.length
        resultSpan.setSpan(
            ForegroundColorSpan(getColor(context, s.code, altCode)),
            begin,
            end,
            Spanned.SPAN_EXCLUSIVE_EXCLUSIVE
        )
        begin = end
    }

    return resultSpan
}

fun createColoredText(context : Context, s : String, color : Int) : CharSequence
{
    val resultSpan = SpannableString(s)
    resultSpan.setSpan(ForegroundColorSpan(color), 0, s.length, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE)
    return resultSpan
}

fun colorResults(context : Context, results : List<TranslateResult>) : CharSequence
{
    if (results.isEmpty())
    {
        return ""
    }

    val splits = mutableListOf<List<SplitAlignment>>()
    for (r in results)
    {
        splits.add(splitAlignment(r.alignedInput, r.alignedResponse))
    }

    var altCode = 0
    var colored = createColoredText(context, splits[0], altCode)
    for (i in 1 until splits.size)
    {
        altCode = (altCode + 1) % 2
        colored = TextUtils.concat(
            colored, "\n\n", createColoredText(context, splits[i], altCode)
        )
    }

    return colored
}

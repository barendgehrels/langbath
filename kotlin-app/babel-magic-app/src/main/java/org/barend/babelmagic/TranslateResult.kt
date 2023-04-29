package org.barend.babelmagic

// Contains via-text and the double-translated ("corrected" / "back") text
data class TranslateResult(
    val viaLanguage : String,
    val viaText : String,
    val backText : String,
    // The score, based on similarity of bare strings, from 0.0 (bad) to 1.0 (perfect)
    // It now also includes word order permutations, but is still based on those bare scores
    val bareScore : Double = 0.0,
    val viaEngine : String = "",
    val backEngine : String = "",
    val alignedInput : EnrichedString = EnrichedString(),
    val alignedResponse : EnrichedString = EnrichedString()
) : Comparable<TranslateResult>
{
    override fun toString() : String
    {
        val engineLabel = when {
            viaEngine.isNotEmpty() && backEngine.isNotEmpty() && viaEngine == backEngine -> " ($backEngine)"
            viaEngine.isNotEmpty() && backEngine.isNotEmpty() -> " ($viaEngine, $backEngine)"
            else -> ""
        }
        return "[${viaLanguage}]: <${viaText}> -> <${backText}>${engineLabel}"
    }
    override fun compareTo(other: TranslateResult): Int
    {
        var result = -this.bareScore.compareTo(other.bareScore)
        return when
        {
            result == 0 -> this.viaLanguage.compareTo(other.viaLanguage)
            else -> result
        }
    }
}

// Contains a string with additional information (errors, movements) per character

package org.barend.babelmagic

const val ENRICHED_CHAR_DEFAULT = 0
const val ENRICHED_CHAR_ALIGNMENT = 1
const val ENRICHED_CHAR_MOVED = 2
const val ENRICHED_CHAR_WRONG_WORD = 3
const val ENRICHED_CHAR_EXTRA = 4

data class EnrichedChar(val c : Char = ' ', val code : Int = 0)

// Class which can be used as a string,
// but keeps some additional information about alignments and moved words.
class EnrichedString(private var mySeq : Array<EnrichedChar> = arrayOf()) : CharSequence
{
    constructor(s: String) : this() {
        mySeq = Array(s.length, { i -> EnrichedChar(s[i])})
    }

    override fun toString() : String
    {
        var result = ""
        for (c in mySeq)
        {
            result += c.c
        }
        return result
    }

    override val length: Int get() = mySeq.size

    override operator fun get(index: Int): Char {
        return mySeq[index].c
    }

    override fun subSequence(startIndex: Int, endIndex: Int): CharSequence
    {
        var result = ""
        for (i in startIndex until endIndex)
        {
            result += mySeq[i].c
        }
        return result
    }

    private fun getCharMarker(code : Int) : Char
    {
        return when(code)
        {
            ENRICHED_CHAR_MOVED -> 'm'
            ENRICHED_CHAR_EXTRA -> 'e'
            ENRICHED_CHAR_WRONG_WORD -> 'w'
            else -> '?'
        }
    }

    fun debugString() : String
    {
        var result = ""
        var oldCode = ENRICHED_CHAR_DEFAULT
        for (c in mySeq)
        {
            // (To avoid adapting all tests) the alignment itself is not marked in the debugstring.
            val newCode = if (c.code == ENRICHED_CHAR_ALIGNMENT) ENRICHED_CHAR_DEFAULT else c.code

            if (oldCode != newCode)
            {
                result += if (newCode != ENRICHED_CHAR_DEFAULT) "[${getCharMarker(newCode)}:" else "]"
                oldCode = newCode
            }
            result += c.c
        }
        if (oldCode != ENRICHED_CHAR_DEFAULT)
        {
            result += "]"
        }
        return result
    }

    fun getChar(index : Int) : EnrichedChar {
        return mySeq[index]
    }

    fun append(s : String, code : Int = 0)
    {
        val oldSize = mySeq.size
        mySeq = Array<EnrichedChar>(mySeq.size + s.length, { i -> if (i < oldSize) mySeq[i] else EnrichedChar(s[i - oldSize], code) })
    }

    fun append(c : Char, code : Int = 0) {
        val oldSize = mySeq.size
        mySeq = Array<EnrichedChar>(oldSize + 1, { i -> if (i < oldSize) mySeq[i] else EnrichedChar(c, code) })
    }

    fun append(s : EnrichedString) {
        val oldSize = mySeq.size
        mySeq = Array<EnrichedChar>(mySeq.size + s.length, { i -> if (i < oldSize) mySeq[i] else s.getChar(i - oldSize) })
    }

    fun append(c : EnrichedChar) {
        val oldSize = mySeq.size
        mySeq = Array<EnrichedChar>(oldSize + 1, { i -> if (i < oldSize) mySeq[i] else c })
    }

    fun prepend(c : EnrichedChar) {
        val oldSize = mySeq.size
        mySeq = Array<EnrichedChar>(oldSize + 1, { i -> if (i == 0) c else mySeq[i - 1]})
    }
}

package org.barend.babelmagic

import org.junit.Test

import org.junit.Assert.*


class AlignWithNeedlemanWunschUnitTest
{
    @Test
    fun testSimplex()
    {

        val r = alignWithNeedlemanWunsch("cat", "katze")

        assertEquals(2, r.size)
        assertEquals(r[0].length, r[1].length)
        assertEquals("cat**", r[0])
        assertEquals("katze", r[1])
    }

    @Test
    fun testOtherAligner()
    {

        val r = alignWithNeedlemanWunsch("cat", "gato", '-')

        assertEquals(2, r.size)
        assertEquals(r[0].length, r[1].length)
        assertEquals("cat-", r[0])
        assertEquals("gato", r[1])
    }

    @Test
    fun testCyrillic()
    {

        val r = alignWithNeedlemanWunsch("кот", "кошка")

        assertEquals(2, r.size)
        assertEquals(r[0].length, r[1].length)
        assertEquals(5, r[0].length)
        assertEquals("ко**т", r[0])
        assertEquals("кошка", r[1])
    }

    @Test
    fun testSentence()
    {

        val r = alignWithNeedlemanWunsch(
            "A gorgeous lion, the king of animals", "A marvellous lion, king of all animals"
        )

        assertEquals(2, r.size)
        assertEquals(r[0].length, r[1].length)
        assertEquals("A gorge**ous lion, the king of**** animals", r[0])
        assertEquals("A marvellous lion,**** king of all animals", r[1])
    }

    @Test
    fun testGapScore()
    {

        // The specified gapscores also depend on differences in the Similarity constants
        var s = listOf(
            "She sells seashells by the seashore", "The seashells she shells while she's here"
        )
        val normal = alignWithNeedlemanWunsch(s[0], s[1], gapScore = -5)
        val large = alignWithNeedlemanWunsch(s[0], s[1], gapScore = -6)
        val huge = alignWithNeedlemanWunsch(s[0], s[1], gapScore = -35)

        // Verify the results are different
        assertNotEquals(normal[0], large[0])
        assertNotEquals(normal[1], large[1])

        assertNotEquals(large[0], huge[0])
        assertNotEquals(large[1], huge[1])
        println(
            "Normal gapscore:\n${normal[0]}\n${normal[1]}" + "\nLarge gapscore:\n${large[0]}\n${large[1]}" + "\nHuge gapscore:\n${huge[0]}\n${huge[1]}"
        )
    }

    @Test
    fun testStringsOfSameLength()
    {

        // Align two different strings, same length, both wrong. Length of both is increased.
        val r = alignWithNeedlemanWunsch(
            "springbock", "spring bok"
        )

        assertEquals(2, r.size)
        assertEquals(r[0].length, r[1].length)
        assertEquals("spring*bock", r[0])
        assertEquals("spring bo*k", r[1])
    }

    @Test
    fun testReversedWord3Alignments()
    {
        // Test with two words of 3 letters reversed. This is the minimum supported,
        // and indicates that SIMILARITY_EQUAL should be 18.
        val r = alignWithNeedlemanWunsch(
            "ten big cod pls", "ten cod big pls"
        )
        assertEquals("ten**** big cod pls", r[0])
        assertEquals("ten cod big**** pls", r[1])
        println("Result \n${r[0]}\n${r[1]}")
    }

    @Test
    fun testReversedWord4Alignments()
    {
        // Because words with 3 letters are supported, words with 4 letters are supported as well
        // This needs SIMILARITY_EQUAL >= 15
        val r = alignWithNeedlemanWunsch(
            "four good fish please", "four fish good please"
        )
        assertEquals("four***** good fish please", r[0])
        assertEquals("four fish good***** please", r[1])
        println("Result \n${r[0]}\n${r[1]}")
    }

    @Test
    fun testReversedWord4AccentedAlignments()
    {
        // Test if accents combined with SIMILARITY_ACCENT have enough weight
        // to support the word reversal, recognizing accent differences
        // This needs SIMILARITY_ACCENT >= 14 to align on canou
        val r = alignWithNeedlemanWunsch(
            "four çáñõů nice please", "four nice canou please"
        )
        assertEquals("four***** çáñõů nice please", r[0])
        assertEquals("four nice canou***** please", r[1])
        println("Result \n${r[0]}\n${r[1]}")
    }


    @Test
    fun testReversedWord5Alignments()
    {
        val r = alignWithNeedlemanWunsch(
            "four bread white please", "four white bread please"
        )
        assertEquals("four****** bread white please", r[0])
        assertEquals("four white bread****** please", r[1])
        println("Result \n${r[0]}\n${r[1]}")
    }

    @Test
    fun testReversedWord5CaseAlignments()
    {
        // Test if accents combined with SIMILARITY_CASE have enough weight
        // to support the word reversal, recognizing case differences
        // and still preferring the 5 letter word i/o the 3 letter word.
        // This needs const val SIMILARITY_CASE >= 15 to align on bread
        val r = alignWithNeedlemanWunsch(
            "four BREAD big please", "four big bread please"
        )
        assertEquals("four**** BREAD big please", r[0])
        assertEquals("four big bread**** please", r[1])
        println("Result \n${r[0]}\n${r[1]}")
    }

}

class PostAlignUnitTest
{
    @Test
    fun testAlignment1()
    {
        val name = object
        {}.javaClass.enclosingMethod.name

        val r = optimizeAlignment("cat**", "katze")

        assertEquals(2, r.size)
        assertEquals(r[0].length, r[1].length)
        println("Result ${name}: ${r[0]} ${r[1]}")
        assertEquals("cat**", r[0])
        assertEquals("katze", r[1])
    }

    @Test
    fun testAlignment2()
    {
        val name = object
        {}.javaClass.enclosingMethod.name

        val r = optimizeAlignment("katze", "cat**")

        assertEquals(2, r.size)
        assertEquals(r[0].length, r[1].length)
        println("Result ${name}: ${r[0]} ${r[1]}")
        assertEquals("katze", r[0])
        assertEquals("cat**", r[1])
    }
}

class AllAlignmentUnitTest
{
    @Test
    fun test1()
    {
        val name = object
        {}.javaClass.enclosingMethod.name

        // Construct text where the last four words are wrong, but get all different processing
        val wrong = "one tow thrie for fivve six"
        val right = "one two three four five"

        val raw = alignWithNeedlemanWunsch(EnrichedString(wrong), EnrichedString(right))
        val word = alignMarkWords(raw[0], raw[1])
        val aligned = optimizeAlignment(word, raw[1])

        val split = splitAlignment(aligned[0], aligned[1])

        println("Results ${name}")
        println(" Raw:\n  ${raw[0]}\n  ${raw[1]}")
        println(" Word:\n  ${word}")
        println(" Post:\n  ${aligned[0]}\n  ${aligned[1]}")
        println(" Split:\n  ${split}\n  ${splitAlignmentToString(split)}")

        // Aligned text (raw), same lengths, too much or not enough letters, or different letters
        assertEquals("one t*ow thrie fo*r fivve six", raw[0].toString())
        assertEquals("one two* three four fi*ve****", raw[1].toString())

        // The second word is marked as bad (50%)
        // The last word is marked as missing
        assertEquals("one t*ow thrie fo*r fivve [e:six]", word.debugString())

        // The alignment optimizer removes some characters but keeps the same lengths.
        // The second one is the one presented to the user. The * stays necessary to indicate
        // that there is a letter too much, or a missing word.
        assertEquals("one t*ow thrie fo*r fivve [e:six]", aligned[0].debugString())
        assertEquals("one two* three four fi*ve****", aligned[1].debugString())

        // The string can be broken up into equal / non equal parts, which can be colored later
        // The full string should be the same as its second input (the correct text)
        assertEquals(aligned[1].toString(), splitAlignmentToString(split))
    }

    @Test
    fun test2()
    {
        val name = object
        {}.javaClass.enclosingMethod.name

        val wrong = "В горах есть дом. Или нет, это не дом, это церковь. Очень красивая картинка"
        val right = "В горах есть дом. Или нет, это не дом, а церковь. Очень красивая фотография."

        val raw = alignWithNeedlemanWunsch(wrong, right)
        val word = alignAdaptWords(raw[0], raw[1])
        val aligned = optimizeAlignment(word, raw[1])

        val split = splitAlignment(aligned[0], aligned[1])

        println("Results ${name}")
        println(" Raw:\n  ${raw[0]}\n  ${raw[1]}")
        println(" Word:\n  ${word}")
        println(" Post:\n  ${aligned[0]}\n  ${aligned[1]}")
        println(" Split:\n  ${split}\n  ${splitAlignmentToString(split)}")
    }

    @Test
    fun test3()
    {
        val name = object
        {}.javaClass.enclosingMethod.name

        val wrong =
            "Я вижу двух девушек, которые едут на велосипедаз на велодорожке вдоль лес. В далеке стоит олень. Они катятся медленно."
        val right =
            "Я вижу двух девушек, едущих на велосипедах по велосипедной дорожке у леса. Вдалеке стоит олень. Они двигаются медленно." // via SL

        val raw = alignWithNeedlemanWunsch(wrong, right)
        val word = alignAdaptWords(raw[0], raw[1])
        val post = optimizeAlignment(word, raw[1])

        val split = splitAlignment(post[0], post[1])

        println("Results ${name}")
        println(" Raw:\n  ${raw[0]}\n  ${raw[1]}")
        println(" Word:\n  ${word}")
        println(" Post:\n  ${post[0]}\n  ${post[1]}")
        println(" Split:\n  ${split}\n  ${splitAlignmentToString(split)}")
    }

    @Test
    fun test4()
    {
        val name = object
        {}.javaClass.enclosingMethod.name

        val wrong = "Of dying, and of being dead"
        val right = "Of dying, and being dead"

        val raw = alignWithNeedlemanWunsch(wrong, right)
        val word = alignAdaptWords(raw[0], raw[1])
        val post = optimizeAlignment(word, raw[1])

        val split = splitAlignment(post[0], post[1])

        println("Results ${name}")
        println(" Raw:\n  ${raw[0]}\n  ${raw[1]}")
        println(" Word:\n  ${word}\n  ${raw[1]}")
        println(" Post:\n  ${post[0]}\n  ${post[1]}")
        println(" Split:\n  ${split}\n  ${splitAlignmentToString(split)}")
    }

    @Test
    fun test5()
    {
        val wrong = "We cannot accept it hesitantly."
        val right = "We can’t accept hesitantly."

        val r = alignWithNeedlemanWunsch(wrong, right, gapScore = -50)

        assertEquals("We cannot accept it hesitantly.", r[0])
        // We would like to see: We can*’t accept*** hesitantly
        // But the tuning of that, or needed code changes, are not found yet.
        assertEquals("We ca*n’t accep***t hesitantly.", r[1])

        println(" Raw:\n  ${r[0]}\n  ${r[1]}")
    }

    private fun allAlign(wrong : String, right : String) : Array<EnrichedString>
    {
        val raw = alignWithSort(EnrichedString(wrong), EnrichedString(right))
        return optimizeAlignment(alignMarkWords(raw[0], raw[1]), raw[1])
    }

    private fun alignerCount(s : EnrichedString) : Int
    {
        var result : Int = 0
        for (c in s)
        {
            if (c == '*' || c == '#')
            {
                result += 1
            }
        }
        return result
    }

    @Test
    fun testPoem()
    {
        val name = object
        {}.javaClass.enclosingMethod.name

        //  Aubade By Philip Larkin
        val right = listOf(
            "I work all day, and get half-drunk at night.",
            "Waking at four to soundless dark, I stare.",
            "In time the curtain-edges will grow light.",
            "Till then I see what’s really always there:",
            "Unresting death, a whole day nearer now,",
            "Making all thought impossible but how",
            "And where and when I shall myself die.",
            "Arid interrogation: yet the dread",
            "Of dying, and being dead,",
            "Flashes afresh to hold and horrify.",
            "The mind blanks at the glare. Not in remorse",
            "—The good not done, the love not given, time",
            "Torn off unused—nor wretchedly because",
            "An only life can take so long to climb",
            "Clear of its wrong beginnings, and may never;",
            "But at the total emptiness for ever,",
            "The sure extinction that we travel to",
            "And shall be lost in always. Not to be here,",
            "Not to be anywhere,",
            "And soon; nothing more terrible, nothing more true.",
            "This is a special way of being afraid",
            "No trick dispels. Religion used to try,",
            "That vast moth-eaten musical brocade",
            "Created to pretend we never die,",
            "And specious stuff that says No rational being",
            "Can fear a thing it will not feel, not seeing",
            "That this is what we fear—no sight, no sound,",
            "No touch or taste or smell, nothing to think with,",
            "Nothing to love or link with,",
            "The anaesthetic from which none come round.",
            "And so it stays just on the edge of vision,",
            "A small unfocused blur, a standing chill",
            "That slows each impulse down to indecision.",
            "Most things may never happen: this one will,",
            "And realisation of it rages out",
            "In furnace-fear when we are caught without",
            "People or drink. Courage is no good:",
            "It means not scaring others. Being brave",
            "Lets no one off the grave.",
            "Death is no different whined at than withstood.",
            "Slowly light strengthens, and the room takes shape.",
            "It stands plain as a wardrobe, what we know,",
            "Have always known, know that we can’t escape,",
            "Yet can’t accept. One side will have to go.",
            "Meanwhile telephones crouch, getting ready to ring",
            "In locked-up offices, and all the uncaring",
            "Intricate rented world begins to rouse.",
            "The sky is white as clay, with no sun.",
            "Work has to be done.",
            "Postmen like doctors go from house to house."
        )
        // Translated by DeepL to Spanish, and back
        val wrong = listOf(
            "I work all day and get drunk at night.",
            "I wake at four o'clock to soundless darkness, and stare.",
            "Eventually, the edges of the curtains will light up.",
            "Until then, I see what has always been there:",
            "Unceasing death, a whole day closer now,",
            "Making all thought impossible, except how",
            "And where and when I myself shall die.",
            "Arid interrogation: yet the fear",
            "Of dying, and of being dead",
            "Flares up again to retain and horrify.",
            "The mind goes blank at the glare. Not in remorse",
            "-The good not done, the love not given, the time",
            "Plucked unused, nor wretched because",
            "A single life can take so long to rise",
            "Clean from its wrongful beginnings, and may never;",
            "But to utter emptiness forever,",
            "The certain extinction to which we travel",
            "And into which we shall ever be lost. To be not here,",
            "To be nowhere,",
            "And soon; nothing more terrible, nothing more true.",
            "This is a special way to be afraid",
            "No trick dispels. Religion used to try,",
            "That vast moth-eaten musical brocade",
            "Created to pretend we never die",
            "And deceitful things that say no rational being",
            "Can fear a thing he doesn't feel, without seeing",
            "That this is what we fear: no sight, no sound,",
            "No touch or taste or smell, nothing to think with,",
            "Nothing to love or bond with,",
            "The anesthesia that no one comes out of.",
            "And so it stays just at the edge of vision,",
            "A tiny unfocused blur, a permanent chill",
            "That slows every impulse to indecision.",
            "Most things may never happen: this one will,",
            "And the realization of it overflows",
            "In boilerplate fear when we're caught with no",
            "Without people or drink. Courage is no good:",
            "It means not scaring others. Being brave",
            "Leaves no one out of the grave.",
            "Death is no more a whimper than an endurance.",
            "Slowly the light grows stronger, and the room takes shape.",
            "It becomes clear as a closet, what we know,",
            "we have always known, we know we cannot escape,",
            "but we cannot accept it. One side will have to go.",
            "Meanwhile the phones crouch, preparing to ring.",
            "In the closed offices, and all the indifferent",
            "The intricately rented world begins to awaken.",
            "The sky is white as clay, sunless.",
            "There is work to be done.",
            "Letter carriers, like doctors, go from house to house."
        )

        assertEquals(wrong.size, right.size)

        val values = mutableListOf<Int>()

        val sep = "\n  "
        var total = 0
        var totalscore = 0
        var totalsplit = 0
        for (i in 0 until wrong.size)
        {
            val a = allAlign(wrong[i], right[i])
            val split = splitAlignment(a[0], a[1])

            assertEquals(a[0].length, a[1].length)

            val score = getDifferencesInAlignedTexts(a[0], a[1])
            val count = alignerCount(a[1])
            total += count
            totalscore += score
            totalsplit += split.size - 1
            values.add(split.size - 1)

            println(" Line:${sep}${wrong[i]}${sep}${a[0]}${sep}${a[1]}  [${score}  ${count}]${sep}${right[i]}")
        }


        println("Total: ${total} ${totalscore} ${totalsplit}-> ${values}")
//        assertEquals(12, total)
//        assertTrue(totalsplit <= 223)
//        assertTrue(totalscore <= 402)

        //10:  Total: 97 503 278-> [5, 10, 14, 8, 8, 2, 4, 1, 4, 9, 5, 4, 5, 4, 6, 6, 6, 18, 7, 0, 4, 0, 0, 1, 8, 8, 2, 0, 2, 16, 2, 4, 5, 0, 6, 7, 2, 0, 3, 12, 12, 6, 12, 9, 6, 11, 6, 2, 5, 1]
        //15:**Total: 97 503 278-> [5, 10, 14, 8, 8, 2, 4, 1, 4, 9, 5, 4, 5, 4, 6, 6, 6, 18, 7, 0, 4, 0, 0, 1, 8, 8, 2, 0, 2, 16, 2, 4, 5, 0, 6, 7, 2, 0, 3, 12, 12, 6, 12, 9, 6, 11, 6, 2, 5, 1]
    }
}

class GetDifferencesInAlignedTextsUnitTest
{
    @Test
    fun testScore1()
    {

        val text = "Ich sehe eine Katze und das Katze seht mir."
        val corrected1 = "Ich sehe eine Katze und die Katze sieht mich."
        val corrected2 = "Ich sehe eine Katze, und die Katze sieht mich."
        val aligned1 = alignWithNeedlemanWunsch(text, corrected1)
        val aligned2 = alignWithNeedlemanWunsch(text, corrected2)
        val score1 = getDifferencesInAlignedTexts(aligned1[0], aligned1[1])
        val score2 = getDifferencesInAlignedTexts(aligned2[0], aligned2[1])
        println("First: ${aligned1[0]}  -> ${aligned1[1]} : ${score1}")
        println("Secnd: ${aligned2[0]}  -> ${aligned2[1]} : ${score2}")
        assertEquals(score1, 5)
        assertEquals(score2, 6)
    }
}


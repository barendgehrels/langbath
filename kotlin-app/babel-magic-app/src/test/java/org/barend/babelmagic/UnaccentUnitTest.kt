package org.barend.babelmagic

import org.junit.Assert
import org.junit.Test

class UnaccentUnitTest
{
    @Test
    fun test()
    {
        // Normal characters
        Assert.assertEquals('*', '*'.unaccent())
        Assert.assertEquals('#', '#'.unaccent())
        Assert.assertEquals('e', 'e'.unaccent())

        // Cases for Spanish, Portuguese, Dutch, Scandinavian, etc
        Assert.assertEquals('a', 'á'.unaccent())
        Assert.assertEquals('e', 'é'.unaccent())
        Assert.assertEquals('i', 'í'.unaccent())
        Assert.assertEquals('o', 'ó'.unaccent())
        Assert.assertEquals('u', 'ů'.unaccent())
        Assert.assertEquals('e', 'ë'.unaccent())
        Assert.assertEquals('o', 'õ'.unaccent())

        Assert.assertEquals('c', 'ç'.unaccent())
        Assert.assertEquals('n', 'ñ'.unaccent())

        // Case is preserved when removing the accent
        Assert.assertEquals('E', 'È'.unaccent())

        // This might go to ss, but unaccent cannot do that. Like õ / oe
        Assert.assertEquals('ß', 'ß'.unaccent())

        // Cyrillic (might look like ASCII but these are different e and ё)
        Assert.assertEquals('е', 'ё'.unaccent())
        Assert.assertEquals('и', 'й'.unaccent())
    }
}

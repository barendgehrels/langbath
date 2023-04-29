package org.barend.babelmagic

import org.junit.Assert.assertEquals
import org.junit.Test

class AzureCompatibleAesEncryptionUnitTest
{
    @Test
    fun test()
    {
        val encryption = AzureCompatibleAesEncryption()
        val encrypted = encryption.encrypt("Hello world", "secret")
        assertEquals("Bsv3u6MYJpz6P3LXkQSziA==", encrypted)
        val decrypted = encryption.decrypt(encrypted, "secret")
        assertEquals("Hello world", decrypted)
    }
}

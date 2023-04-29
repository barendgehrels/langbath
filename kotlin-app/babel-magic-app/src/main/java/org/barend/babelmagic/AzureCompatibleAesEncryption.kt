// Contains the encryption/decryption compatable to the Azure service
// Adapted from: https://www.knowband.com/blog/tips/how-to-setup-encryption-and-decryption-between-android-and-asp-net-web-application/

package org.barend.babelmagic

import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.IvParameterSpec
import javax.crypto.spec.SecretKeySpec

// If this integer is set with a magic value, there is no encryption.
// This is only used in unit tests. The app itself always encrypts.
var VERSION_CODE_WITHOUT_ENCRYPTION = -1

class AzureCompatibleAesEncryption
{
    private val characterEncoding = "UTF-8"
    private val cipherTransformation = "AES/CBC/PKCS5Padding"
    private val aesEncryptionAlgorithm = "AES"
    private val encoder = Base64.getEncoder()
    private val decoder = Base64.getDecoder()

    fun encrypt(plainText : String, key : String) : String
    {
        val keyBytes = getKeyBytes(key)
        val encrypted = encrypt(plainText.toByteArray(charset(characterEncoding)), keyBytes, keyBytes)
        return encoder.encodeToString(encrypted)
    }

    fun decrypt(encryptedText : String, key : String) : String
    {
        try
        {
            val cipheredBytes = decoder.decode(encryptedText)
            val keyBytes = getKeyBytes(key)
            return String(decrypt(cipheredBytes, keyBytes, keyBytes))
        }
        catch(ex : Exception)
        {
            return ""
        }
    }

    private fun decrypt(cipherText : ByteArray, key : ByteArray, initialVector : ByteArray) : ByteArray
    {
        val cipher : Cipher = Cipher.getInstance(cipherTransformation)
        val secretKeySpecy = SecretKeySpec(key, aesEncryptionAlgorithm)
        val ivParameterSpec = IvParameterSpec(initialVector)
        cipher.init(Cipher.DECRYPT_MODE, secretKeySpecy, ivParameterSpec)
        return cipher.doFinal(cipherText)
    }

    private fun encrypt(plainText : ByteArray, key : ByteArray, initialVector : ByteArray) : ByteArray
    {
        val cipher : Cipher = Cipher.getInstance(cipherTransformation)
        val secretKeySpec = SecretKeySpec(key, aesEncryptionAlgorithm)
        val ivParameterSpec = IvParameterSpec(initialVector)
        cipher.init(Cipher.ENCRYPT_MODE, secretKeySpec, ivParameterSpec)
        return cipher.doFinal(plainText)
    }

    private fun getKeyBytes(key : String) : ByteArray
    {
        val keyBytes = ByteArray(16)
        val parameterKeyBytes = key.toByteArray(charset(characterEncoding))
        System.arraycopy(
            parameterKeyBytes,
            0,
            keyBytes,
            0,
            Math.min(parameterKeyBytes.size, keyBytes.size)
        )
        return keyBytes
    }
}

fun encryptedTimestamp(key : String, timeOffset : Int = 0) : String
{
    val currentTimestamp = timeOffset + System.currentTimeMillis() / 1000
    val encryption = AzureCompatibleAesEncryption()
    return encryption.encrypt(currentTimestamp.toString(), key)
}

fun encryptedInput(input : String, key : String) : String
{
    val encryption = AzureCompatibleAesEncryption()
    return encryption.encrypt(input, key)
}

fun encryptedInput(text : String, key : String, versionCode : Int) : String
{
    if (versionCode == VERSION_CODE_WITHOUT_ENCRYPTION) { return text; }
    val encryption = AzureCompatibleAesEncryption()
    return encryption.encrypt(text, key)
}

fun decryptedOutput(text : String, key : String, versionCode : Int) : String
{
    if (versionCode == VERSION_CODE_WITHOUT_ENCRYPTION) { return text; }
    val encryption = AzureCompatibleAesEncryption()
    return encryption.decrypt(text, key)
}
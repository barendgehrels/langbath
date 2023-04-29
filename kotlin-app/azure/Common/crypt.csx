// Encryption/decryption code
// The main objective was to get it on Azure in C#, identical to Android in Kotlin.
// As described here:
// From: https://www.knowband.com/blog/tips/how-to-setup-encryption-and-decryption-between-android-and-asp-net-web-application/

using System;
using System.Text;
using System.Security.Cryptography;

public static long GetTimeDifference(string timestamp, string key)
{
    long clientTimestamp = Int32.Parse(Decrypt(timestamp, key));
    long unixTime = ((DateTimeOffset)DateTime.UtcNow).ToUnixTimeSeconds();
    return unixTime - clientTimestamp;
}

public static string Encrypt(String plainText, String key)
{
    var plainBytes = Encoding.UTF8.GetBytes(plainText);
    return Convert.ToBase64String(Encrypt(plainBytes, GetRijndaelManaged(key)));
}

public static string Decrypt(String encryptedText, String key)
{
    var encryptedBytes = Convert.FromBase64String(encryptedText);
    return Encoding.UTF8.GetString(Decrypt(encryptedBytes, GetRijndaelManaged(key)));
}

private static byte[] Encrypt(byte[] plainBytes, RijndaelManaged rijndaelManaged)
{
    return rijndaelManaged.CreateEncryptor()
        .TransformFinalBlock(plainBytes, 0, plainBytes.Length);
}

private static byte[] Decrypt(byte[] encryptedData, RijndaelManaged rijndaelManaged)
{
    return rijndaelManaged.CreateDecryptor()
        .TransformFinalBlock(encryptedData, 0, encryptedData.Length);
}

private static RijndaelManaged GetRijndaelManaged(String secretKey)
{
    var keyBytes = new byte[16];
    var secretKeyBytes = Encoding.UTF8.GetBytes(secretKey);
    Array.Copy(secretKeyBytes, keyBytes, Math.Min(keyBytes.Length, secretKeyBytes.Length));
    return new RijndaelManaged
    {
        Mode = CipherMode.CBC,
        Padding = PaddingMode.PKCS7,
        KeySize = 128,
        BlockSize = 128,
        Key = keyBytes,
        IV = keyBytes
    };
}

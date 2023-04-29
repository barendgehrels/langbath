#load "..\Common\crypt.csx"

using System;
using System.Net;
using System.Text;
using Microsoft.AspNetCore.Mvc;

// ------------------------------------------------------------------------------------------
// Functionality for conversions
// ------------------------------------------------------------------------------------------
public static int ConvertToInteger(string Value)
{
    if (string.IsNullOrEmpty(Value))
    {
        return 0;
    }

    try
    {
        return Int32.Parse(Value);
    }
    catch (FormatException)
    {
        return 0;
    }
}

public static double ConvertToDouble(string Value)
{
    if (Value == null)
    {
        return 0;
    }

    double OutVal;
    double.TryParse(Value, out OutVal);

    if (double.IsNaN(OutVal) || double.IsInfinity(OutVal))
    {
        return 0;
    }
    return OutVal;
}

// Structure used by all services for settings.
// Most of them are read from the Azure "Application Settings" in 
// Azure, Function App, Configuration
public static class Settings
{
  private static string ephemeral_key = Environment.GetEnvironmentVariable("EPHEMERAL_KEY");
  private static string text_key = Environment.GetEnvironmentVariable("TEXT_KEY");
  private static string eternal_key = Environment.GetEnvironmentVariable("ETERNAL_KEY");
  private static string version_code_without_encryption = Environment.GetEnvironmentVariable("VERSION_CODE_WITHOUT_ENCRYPTION");

  private static string azure_translate_key = Environment.GetEnvironmentVariable("AZURE_TRANSLATE_API_KEY");
  private static string azure_region = Environment.GetEnvironmentVariable("AZURE_TRANSLATE_REGION");
  private static string azure_tts_key = Environment.GetEnvironmentVariable("AZURE_TTS_API_KEY");

  private static string deepl_key = Environment.GetEnvironmentVariable("DEEPL_AUTH_KEY");
  private static string unsplash_key = Environment.GetEnvironmentVariable("UNSPLASH_CLIENT_ID");

  // Deprecated
  public static string Key() { return ephemeral_key; }

  // Used for encryption of the timestamp
  public static string EphemeralKey() { return ephemeral_key; }
  public static string TextKey() { return text_key; }
  public static string EternalKey() { return eternal_key; }

  public static string AzureTranslateKey() { return azure_translate_key; }
  public static string AzureTextToSpeechKey() { return azure_tts_key; }
  public static string AzureTranslateRegion() { return azure_region; }

  public static string DeeplAuthKey() { return deepl_key; }
  public static string UnsplashClientId() { return unsplash_key; }

  public static string TextDecodingErrorTag() { return "@!"; }
  public static long TimestampDecodingErrorTag() { return -999999; }

  public static int VersionCodeWithoutEncryption()
  {
      return ConvertToInteger(version_code_without_encryption);
  }

  public static int TextLengthMin() { return 10; }
  public static int TextLengthMax() { return 10 * 42; }

  // This is about modified text. It can be a bit longer or shorter.
  public static int TextLengthMinTts() { return TextLengthMin() / 2; }
  public static int TextLengthMaxTts() { return TextLengthMax() * 2; }

  public static int TextLengthMinChinese() { return 3; }
}

private static bool IsChineseChar(char c)
{
    return (uint)c >= 0x4E00 && (uint)c <= 0x2FA1F;
}

public static bool IsChinese(string text)
{
    if (text.Length >= 3)
    {
        return IsChineseChar(text[0]);
    }
    return false;
}

// There should be some text (a few words) to be able to detect the language automatically.
// But protect the services from too much text (also to save costs / avoid hacks).
// The UI also contains this value.
public static string CheckTextLength(string text)
{
    var minLength = IsChinese(text) ? Settings.TextLengthMinChinese() : Settings.TextLengthMin();
    if (text.Length < minLength)
    {
        return "Text is too short.";
    }
    if (text.Length > Settings.TextLengthMax())
    {
      return "Text is too long.";
    }
    return "";
}

public static string CheckClientVersion(int versionCode)
{
    if (versionCode != Settings.VersionCodeWithoutEncryption() && versionCode < 1)
    {
        return $"Version {versionCode} not supported";
    }
    return "";
}

public static string EncryptedText(string text, int versionCode)
{
    if (string.IsNullOrEmpty(text) || versionCode == Settings.VersionCodeWithoutEncryption())
    {
        return text;
    }
    return Encrypt(text, Settings.TextKey());
}

public static string GetInputText(string text, int versionCode)
{
    if (string.IsNullOrEmpty(text) || versionCode == Settings.VersionCodeWithoutEncryption())
    {
        return text;
    }
    try
    {
        return Decrypt(text, Settings.TextKey());
    }
    catch
    {
        return Settings.TextDecodingErrorTag();
    }
}

public static long GetTimeDifferenceInSeconds(string timestamp)
{
    if (timestamp == Settings.EternalKey())
    {
        // To test from Azure.
        return 0;
    }

    try
    {
        long clientTimestamp = Int32.Parse(Decrypt(timestamp, Settings.EphemeralKey()));
        long unixTime = ((DateTimeOffset)DateTime.UtcNow).ToUnixTimeSeconds();
        return Math.Abs(unixTime - clientTimestamp);
    }
    catch
    {
        return Settings.TimestampDecodingErrorTag();
    }
}

public static string CheckTimeDifference(string timestamp)
{
    long diff = GetTimeDifferenceInSeconds(timestamp);
    if (diff == Settings.TimestampDecodingErrorTag())
    {
      return "Decoding not successful";
    }

    return diff > 60 ? "Time elapsed" : "";
}

// ------------------------------------------------------------------------------------------
// Functionality for returning errors
// ------------------------------------------------------------------------------------------
public struct ErrorNotification
{
    public ErrorNotification(string e)
    {
        error = e;
    }

    public string error { get; }
}

public static OkObjectResult ErrorResult(string message)
{
    return new OkObjectResult(new ErrorNotification(message));
}

// ------------------------------------------------------------------------------------------
// Functionality for translations, language maps, corresponding voices and codes.
// ------------------------------------------------------------------------------------------

public enum Engine { Microsoft, DeepL, Mock }

// Contains via-languages (for example "es", "it" (Spanish, Italian) for "fr"), the language tag ("fr-FR"),
// the engine (Microsoft, DeepL, Google, Yandex, ...) and the voices to be used to speak the text.
// See also https://en.wikipedia.org/wiki/Language_localisation
// and https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
public struct LangInfo
{
    public LangInfo(string[] v, string s, Engine e, string[] vc)
    {
        vias = v;
        subcode = s;
        engine = e;
        voices = vc;
    }
    public string[] vias { get; }
    public string subcode { get; }
    public Engine engine { get; }
    public string[] voices { get; }
}

// See https://docs.microsoft.com/en-us/azure/cognitive-services/speech-service/language-support?tabs=stt-tts#text-to-speech
// Creates a map from ISO 639-1 language codes (two letter, lowercase)
public static Dictionary<string, LangInfo> GetLanguageInfoMap()
{
    var d = Engine.DeepL;
    var m = Engine.Microsoft;
    // var d = Engine.Mock;
    // var m = Engine.Mock;
    return new Dictionary<string, LangInfo>(){
            {"en", new LangInfo(new string[]{"da", "es"}, "en-GB", d,
                new string[]{"Abbi", "Alfie", "Bella", "Elliot", "Ethan", "Hollie", "Libby", "Noah",
                             "Oliver", "Olivia", "Ryan", "Sonia", "Thomas"})},
            {"nl", new LangInfo(new string[]{"de", "da"}, "nl-NL", d,
                new string[]{"Colette", "Fenna", "Maarten"})},
            {"de", new LangInfo(new string[]{"nl", "da"}, "de-DE", d,
                new string[]{"Amala", "Bernd", "Christoph", "Conrad", "Elke", "Gisela", "Kasper",
                             "Katja", "Killian", "Klarissa", "Klaus", "Louisa", "Maja", "Ralf", "Tanja"})},

            {"es", new LangInfo(new string[]{"fr", "it"}, "es-ES", d,
                new string[]{"Alvaro", "Elvira"})},
            {"fr", new LangInfo(new string[]{"es", "it"}, "fr-FR", d,
                new string[]{"Alain", "Brigitte", "Celeste", "Claude", "Coralie", "Denise", "Henri",
                             "Jacqueline", "Jerome", "Josephine", "Maurice", "Yves", "Yvette"})},
            {"it", new LangInfo(new string[]{"es", "fr"}, "it-IT", d,
                new string[]{"Diego", "Elsa", "Isabella", "Rinaldo"})},
            {"ca", new LangInfo(new string[]{"es", "fr"}, "ca-ES", m,
                new string[]{"Alba","Enric", "Joana"})},

            {"cs", new LangInfo(new string[]{"sk", "pl"}, "cs-CZ", d,
                new string[]{"Antonin", "Vlasta"})},
            {"uk", new LangInfo(new string[]{"ru", "pl"}, "uk-UA", d,
                new string[]{"Ostap", "Polina"})},
            {"ru", new LangInfo(new string[]{"uk", "pl", "sl"}, "ru-RU", d,
                new string[]{"Dariya", "Dmitry", "Svetlana"})},

            {"ar", new LangInfo(new string[]{"fr", "fa"}, "ar-IQ", m,
                new string[]{"Bassel", "Rana"})},
            {"zh", new LangInfo(new string[]{"fr", "en"}, "zh-CN", m,
                new string[]{"Xiaochen", "Yunhao"})},

            {"sw", new LangInfo(new string[]{"es", "fr"}, "sw-KE", m,
                new string[]{"Rafiki","Zuri"})}

            };
}

// Serializable structure to generate JSON input for MS translate
public struct TranslationInput
{
    public TranslationInput(string t)
    {
        text = t;
    }
    public string text { get; }
}

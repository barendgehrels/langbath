// TranslateVia
// This is the main function of the app.
// It translates a text to a related language, and then translates it back.
// This way, typos are fixed, wrong word order is fixed, and other errors can be fixed too.
// But it can also change things which are not errors. However, the user can judge it.
// Apart from these two translations, it can also translate to the user's native language
// (to be able to see what (s)he really wrote).
// There are three versions, which can be specified with a parameter "Engine":
// - a mock (to test), just returning fixed info from my website
// - using DeepL
// - using Microsoft

#r "Newtonsoft.Json"
#load "..\Common\common.csx"
#load "..\Common\crypt.csx"

using System;
using System.Net;
using System.Text;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Primitives;
using Newtonsoft.Json;

private struct Translation
{
    public Translation(string c, string t, string e)
    {
        code = c;
        text = t;
        engine = e;
    }

    public string code { get; }
    public string text { get; set; }
    public string engine { get; }
}

private struct TranslationResponse
{
    public Translation via { get; set; }
    public Translation back { get; set; }
    public Translation native { get; set; }
}

private static async Task<Translation> TranslateWithMock(string sourceLanguage, string targetLanguage, string inputText, ILogger log)
{
    string lang = targetLanguage.ToUpper();
    var uri = $"https://barend.home.xs4all.nl/babel-magic/deepl_mock_{lang}.json";
    try
    {
        using var client = new HttpClient();
        var response = await client.GetAsync(uri);
        var jsondata = await response.Content.ReadAsStringAsync();

        //log.LogInformation($"target: {lang} json: {jsondata}");

        dynamic json = Newtonsoft.Json.JsonConvert.DeserializeObject(jsondata);
        string text = json.translations[0].text;
        return new Translation(targetLanguage, text, "mock");
    }
    catch (Exception ex)
    {
        return new Translation("", $"Exception: {ex.Message}", "mock");
    }
}

private static async Task<Translation> TranslateWithDeepL(string sourceLanguage, string targetLanguage, string inputText, ILogger log)
{
    string authKey = Settings.DeeplAuthKey();
    string lang = targetLanguage.ToUpper();
    var uri = $"https://api-free.deepl.com/v2/translate?target_lang={lang}&auth_key={authKey}&text={inputText}";
    try
    {
        using var client = new HttpClient();
        var response = await client.GetAsync(uri);
        var jsondata = await response.Content.ReadAsStringAsync();

        dynamic json = Newtonsoft.Json.JsonConvert.DeserializeObject(jsondata);
        string text = json.translations[0].text;
        return new Translation(targetLanguage, text, "dl");
    }
    catch (Exception ex)
    {
        return new Translation("", $"Exception: {ex.Message}", "dl");
    }
}

private static async Task<Translation> TranslateWithMicrosoft(string sourceLanguage, string targetLanguage, string inputText, ILogger log)
{
    var uri = $"https://api.cognitive.microsofttranslator.com/translate?api-version=3.0&to={targetLanguage}";
    var input = new TranslationInput(inputText);
    var jsonInput = $"[{JsonConvert.SerializeObject(input)}]";
    var data = new StringContent(jsonInput, Encoding.UTF8, "application/json");

    using var client = new HttpClient();
    client.DefaultRequestHeaders.Add("Ocp-Apim-Subscription-Key", Settings.AzureTranslateKey());
    client.DefaultRequestHeaders.Add("Ocp-Apim-Subscription-Region", Settings.AzureTranslateRegion());
    var response = await client.PostAsync(uri, data);
    var jsondata = await response.Content.ReadAsStringAsync();   

    try
    {
        // Response is like:
        // [{"detectedLanguage":{"language":"en","score":1.0},"translations":[{"text":"Здравствуйте, как вас зовут? Меня зовут Себастьян.","to":"ru"}]}]
        dynamic json = Newtonsoft.Json.JsonConvert.DeserializeObject(jsondata);
        string translation = json[0].translations[0].text;
        return new Translation(targetLanguage, translation, "ms");
    }
    catch (Exception ex)
    {
        return new Translation("??", $"Exception: {ex.Message} in {jsondata}", "ms");
    }
}

private static async Task<Translation> TranslateRequest(Engine engine, bool mock, 
        string sourceLanguage, string targetLanguage, string inputText, ILogger log)
{
    if (! mock)
    {
        switch(engine)
        {
            case Engine.Microsoft : return await TranslateWithMicrosoft(sourceLanguage, targetLanguage, inputText, log);
            case Engine.DeepL : return await TranslateWithDeepL(sourceLanguage, targetLanguage, inputText, log);
        }
    }
    return await TranslateWithMock(sourceLanguage, targetLanguage, inputText, log);
}

private static Translation EncryptedTranslation(Translation tr, int versionCode)
{
    return new Translation(tr.code, EncryptedText(tr.text, versionCode), tr.engine);
}

public static OkObjectResult ServiceErrorResult(string message)
{
    return new OkObjectResult(new ErrorNotification($"[TV] {message}"));
}

private struct RequestParameters
{
    public int version { get; set; }
    public string timestamp { get; set; }
    // Source/target language (this is the language the user is learning)
    public string lang { get; set; }
    // Via language (related to the target language)
    public string via { get; set; }
    // Native language (optional, for the user to check his text)
    public string native { get; set; }
    // The encrypted text to translate to via, and to translate back
    public string text { get; set; }
    public bool mock { get; set; }
}

public static async Task<IActionResult> Run(HttpRequest req, ILogger log)
{
    RequestParameters par;

    try
    {
        string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
        par = JsonConvert.DeserializeObject<RequestParameters>(requestBody);
    }
    catch (Exception ex)
    {
      return ServiceErrorResult($"Error in parsing data: '{ex.Message}'");
    }

    string check = CheckClientVersion(par.version);
    if (!string.IsNullOrEmpty(check))
    {
        return ServiceErrorResult(check);
    }

    if (string.IsNullOrEmpty(par.timestamp) 
        || string.IsNullOrEmpty(par.lang)
        || string.IsNullOrEmpty(par.via)
        || string.IsNullOrEmpty(par.text))
    {
      return ServiceErrorResult("Pass parameters for timestamp, lang, via, text");
    }

    check = CheckTimeDifference(par.timestamp);
    if (!string.IsNullOrEmpty(check))
    {
      return ServiceErrorResult(check);
    }

    string text = GetInputText(par.text, par.version);
    if (text == Settings.TextDecodingErrorTag())
    {
        return ServiceErrorResult("Decoding of input text not successful");
    }

    check = CheckTextLength(text);
    if (!string.IsNullOrEmpty(check))
    {
      return ServiceErrorResult(check);
    }

    var map = GetLanguageInfoMap();

    if (!map.ContainsKey(par.lang))
    {
      return ServiceErrorResult($"Language '{par.lang}' not supported.");
    }
 
    var info = map[par.lang];

    bool found = false;
    foreach(string code in info.vias)
    {
        if (par.via == code)
        {
            found = true;
            break;
        }
    }
    if (! found)
    {
      return ServiceErrorResult($"Language via '{par.via}' not supported for {par.lang}.");
    }
    // log.LogInformation($"Language={lang} native={native} vias={info.vias.Length} textlen={text.Length}");

    try
    {
        var response = new TranslationResponse();

        response.via = await TranslateRequest(info.engine, par.mock, par.lang, par.via, text, log);
        if (string.IsNullOrEmpty(response.via.code))
        {
            return ServiceErrorResult(response.via.text);
        }
        response.back = await TranslateRequest(info.engine, par.mock, par.via, par.lang, response.via.text, log);
        if (string.IsNullOrEmpty(response.back.code))
        {
            return ServiceErrorResult(response.back.text);
        }

        if (! string.IsNullOrEmpty(par.native) && map.ContainsKey(par.native) && info.engine != Engine.Microsoft)
        {
            // Only translate to native if the language is DeepL.
            // DeepL can overwrite the native translation with a (sometimes) better one.
            // In other cases, translation to native is already done when detecting the language.

            // Save, possibly, a request to the translation service
            if (response.via.code == par.native)
            {
                response.native = response.via;
            }
            else if (response.back.code == par.native)
            {
                response.native = response.back;
            }
            else
            {
                response.native = await TranslateRequest(info.engine, par.mock, par.lang, par.native, text, log);
                if (string.IsNullOrEmpty(response.native.code))
                {
                    return ServiceErrorResult(response.native.text);
                }
            }
        }

        response.via = EncryptedTranslation(response.via, par.version);
        response.back = EncryptedTranslation(response.back, par.version);
        response.native = EncryptedTranslation(response.native, par.version);

        return new OkObjectResult(response);
    }
    catch (Exception ex)
    {
      return ServiceErrorResult($"Translation not successful: '{ex.Message}'");
    }
}


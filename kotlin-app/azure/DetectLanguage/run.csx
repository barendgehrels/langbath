// DetectLanguage
// This function is used to detect the language in which the user wrote the text.
// There are several possibilies
// - microsoft (currently used - but not ideal yet)
// - deepL (just translate, automatically)
// - "LanguageTool.org" (I started with this)

#r "Newtonsoft.Json"
#load "..\Common\common.csx"
#load "..\Common\crypt.csx"

using System;
using System.Net;
using System.Text;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Primitives;
using Newtonsoft.Json;

// Serializable structure containing the language detection result
private struct Detection
{
    public Detection(string c, double s, string t)
    {
        code = c;
        score = s;
        text = t;
        supported = false;
        via = new string[]{};
    }

    public bool supported { get; set; }
    public string code { get; }
    public string[] via { get; set; }
    public double score { get; }
    public string text { get; set; }
}

private static async Task<Detection> MakeRequest(string inputText, string nativeLanguage, ILogger log)
{
    string jsondata = "";

    try
    {
        // Using "translate we get a translation back in the same go.
        // Alternatively, it's possible to use "detect"
        // But the score or detection are not influenced by that.
        // The score is often 1.0, even for Russian nonsense words.
        var uri = $"https://api.cognitive.microsofttranslator.com/translate?api-version=3.0&to={nativeLanguage}";
        var input = new TranslationInput(inputText);
        var jsonInput = $"[{JsonConvert.SerializeObject(input)}]";
        var data = new StringContent(jsonInput, Encoding.UTF8, "application/json");

        using var client = new HttpClient();
        client.DefaultRequestHeaders.Add("Ocp-Apim-Subscription-Key", Settings.AzureTranslateKey());
        client.DefaultRequestHeaders.Add("Ocp-Apim-Subscription-Region", Settings.AzureTranslateRegion());
        var response = await client.PostAsync(uri, data);
        jsondata = await response.Content.ReadAsStringAsync();
    }
    catch (Exception ex)
    {
        return new Detection("", 0.0, $"Exception: {ex.Message}");
    }

    try
    {
        // Response is like:
        // [{"detectedLanguage":{"language":"en","score":1.0},"translations":[{"text":"Здравствуйте.","to":"ru"}]}]
        dynamic json = Newtonsoft.Json.JsonConvert.DeserializeObject(jsondata);
        try
        {
            if (json.ContainsKey("error"))
            {
                // Propagate the error, including the target language
                string msg = json.error.message;
                return new Detection("", 0.0, $"Translate service with '{nativeLanguage}' returns: '{msg}'");
            }
        }
        catch (Exception ex) {}

        string scoreField = json[0].detectedLanguage.score;
        string language = json[0].detectedLanguage.language;
        string translation = json[0].translations[0].text;
        language = (language.Length > 2 ? language.Substring(0, 2) : language).ToLower();
        return new Detection(language, ConvertToDouble(scoreField), translation);
    }
    catch (Exception ex)
    {
        return new Detection("", 0.0, $"Exception: {ex.Message}, json: {jsondata}");
    }
}

private static async Task<Detection> DetectRequest(bool mock, string inputText, string nativeLanguage, ILogger log)
{
    if (mock)
    {
        return new Detection("es", 0.93, $"Hi! ({inputText})");
    }
    return await MakeRequest(inputText, nativeLanguage, log);
}

public static OkObjectResult ServiceErrorResult(string message)
{
    return new OkObjectResult(new ErrorNotification($"[DL] {message}"));
}

private struct RequestParameters
{
    public int version { get; set; }
    public string timestamp { get; set; }
    public string native { get; set; }
    public string text { get; set; }
    public bool mock { get; set; }
}

public static async Task<IActionResult> Run(HttpRequest req, ILogger log)
{
    // log.LogInformation("Babel-Magic by Barend - C# HTTP function 'DetectLanguage'.");

    RequestParameters par;

    try
    {
        string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
        par = JsonConvert.DeserializeObject<RequestParameters>(requestBody);
    }
    catch (Exception ex)
    {
      return ServiceErrorResult($"Parsing data: '{ex.Message}'");
    }

    string check = CheckClientVersion(par.version);
    if (!string.IsNullOrEmpty(check))
    {
        return ServiceErrorResult(check);
    }

    check = CheckTimeDifference(par.timestamp);
    if (!string.IsNullOrEmpty(check))
    {
      return ServiceErrorResult(check);
    }

    if (string.IsNullOrEmpty(par.timestamp) || string.IsNullOrEmpty(par.native) || string.IsNullOrEmpty(par.text))
    {
      return ServiceErrorResult("Pass parameters for timestamp, native, text.");
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

    try
    {
      var detection = await DetectRequest(par.mock, text, par.native, log);
      if (string.IsNullOrEmpty(detection.code))
      {
          return ServiceErrorResult(detection.text);
      }

      var map = GetLanguageInfoMap();
      detection.supported = map.ContainsKey(detection.code);
      if (detection.supported)
      {
          detection.via = map[detection.code].vias;
      }
      detection.text = EncryptedText(detection.text, par.version);

      return new OkObjectResult(detection);
    }
    catch (Exception ex)
    {
      return ServiceErrorResult($"Request not successful: '{ex.Message}'");
    }
}

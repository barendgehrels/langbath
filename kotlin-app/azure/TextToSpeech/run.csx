// Text to speech
// This function calls the Microsoft Text To Speech service (which is another Azure service)
// The input text is encrypted, no raw texts pass the service.
// The output mp3 is sent in base64 form (not encrypted)

#r "Newtonsoft.Json"
#load "..\Common\common.csx"
#load "..\Common\crypt.csx"

using System;
using System.Net;
using System.Text;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Primitives;
using Newtonsoft.Json;

private struct SpeechInfo
{
    public SpeechInfo(string c, string v, string b)
    {
        code = c;
        voice = v;
        bytes = b;
    }

    public string code { get; }
    public string voice { get; }
    public string bytes { get; }
}

// Adapted from
// https://stackoverflow.com/questions/56690700/how-to-handle-long-lasting-microsoft-cognitive-services-text-to-speech-calls-in
private static async Task<SpeechInfo> MakeRequest(string text)
{
    string host = "https://westeurope.tts.speech.microsoft.com/cognitiveservices/v1";
    using (var client = new HttpClient())
    {
        using (var request = new HttpRequestMessage())
        {
            request.Method = HttpMethod.Post;
            request.RequestUri = new Uri(host);
            request.Content = new StringContent(text, Encoding.UTF8, "application/ssml+xml");

            request.Headers.Add("Ocp-Apim-Subscription-Key", Settings.AzureTextToSpeechKey());
            request.Headers.Add("User-Agent", "Babel-Magic");
            request.Headers.Add("X-Microsoft-OutputFormat", "audio-16khz-128kbitrate-mono-mp3");

            using (var response = await client.SendAsync(request))
            {
                response.EnsureSuccessStatusCode();
                using (var dataStream = await response.Content.ReadAsStreamAsync())
                {
                    byte[] bytes = new byte[dataStream.Length];
                    dataStream.Read(bytes, 0, (int)dataStream.Length);
                    return new SpeechInfo("-", "-", Convert.ToBase64String(bytes));
                }
            }
        }
    }
}
 
 // Builds a SSML (Speech Synthesis Markup Language) request from (not encrypted) text
 // in a specified language.
private static string BuildSsml(Dictionary<string, LangInfo> map, string text, string lang)
{
    var info = map[lang];

    // Select a random voice for the requested language.
    long unixTime = ((DateTimeOffset)DateTime.UtcNow).ToUnixTimeSeconds();
    long voiceIndex = unixTime % info.voices.Length;

    // Build final voice-tag like "ru-RU-DmitryNeural"
    var voice = $"{info.subcode}-{info.voices[voiceIndex]}Neural";

    // Be sure to avoid invalid XML (TODO: this is not complete)
    var adaptedText = text.Replace('"', ' ').Replace('<', ' ').Replace('>', ' ').Replace('/', ' ').Replace(':', ' ');

    var voiceEntry = $"xml:lang='{info.subcode}' name='{voice}'";
    var xmlHeader = "version='1.0' xml:lang='en-US'";
    var data = $"<speak {xmlHeader}><voice {voiceEntry}>{adaptedText}</voice></speak>";
    return data;
}

public static OkObjectResult ServiceErrorResult(string message)
{
    return new OkObjectResult(new ErrorNotification($"[TTS] {message}"));
}

private struct RequestParameters
{
    public int version { get; set; }
    public string timestamp { get; set; }
    public string lang { get; set; }
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

    if (string.IsNullOrEmpty(par.timestamp) || string.IsNullOrEmpty(par.lang) || string.IsNullOrEmpty(par.text))
    {
      return ServiceErrorResult("Pass parameters for timestamp, lang, text.");
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

    if (text.Length < Settings.TextLengthMinTts())
    {
      return ServiceErrorResult("Text is too short.");
    }
    if (text.Length > Settings.TextLengthMaxTts())
    {
      return ServiceErrorResult("Text is too long.");
    }

    var map = GetLanguageInfoMap();
    if (!map.ContainsKey(par.lang))
    {
        return ServiceErrorResult($"Language '{par.lang}' not supported.");
    }

    try
    {
        string ssml = BuildSsml(map, text, par.lang);
        var detection = await MakeRequest(ssml);
        return new OkObjectResult(detection);
    }
    catch (Exception ex)
    {
      return ServiceErrorResult($"Request not successful: '{ex.Message}'");
    }
}

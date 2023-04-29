// Initialize
// This function is used to give the clients all the function keys (FK), in encrypted form.
// These keys themselves are stored in the Application Settings.
// The client has to call this once, and can then call all other functions with the appropriate FK's
// It also checks the time difference (time on client, time on server), which could be a few seconds.
// It also returns a new user id (if not passed by the client). This is only used for logging purposes.

#r "Newtonsoft.Json"
#load "..\Common\common.csx"
#load "..\Common\crypt.csx"

using System;
using System.Net;
using System.Text;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Primitives;
using Newtonsoft.Json;

// Serializable structure with initialization information.
private struct Initialization
{
    private static string fk_key = Environment.GetEnvironmentVariable("FK_KEY");
    private static string env_tts = Environment.GetEnvironmentVariable("FK_TTS");
    private static string env_dl = Environment.GetEnvironmentVariable("FK_DL");
    private static string env_tv = Environment.GetEnvironmentVariable("FK_TV");
    private static string env_ri = Environment.GetEnvironmentVariable("FK_RI");

    public Initialization(string id, long diff)
    {
        userid = id;
        ts_diff = diff;
        fk_tts = Encrypt(env_tts, fk_key);
        fk_dl = Encrypt(env_dl, fk_key);
        fk_tv = Encrypt(env_tv, fk_key);
        fk_ri = Encrypt(env_ri, fk_key);
    }

    public string userid { get; }

    public long ts_diff { get; }

    // Function keys for: TextToSpeech, DetectLanguage, TranslateVia and RandomImage
    public string fk_tts { get; }
    public string fk_dl { get; }
    public string fk_tv { get; }
    public string fk_ri { get; }
}


public static OkObjectResult ServiceErrorResult(string message)
{
    return new OkObjectResult(new ErrorNotification($"[INIT] {message}"));
}

private struct RequestParameters
{
    public int version { get; set; }
    public string timestamp { get; set; }
    public string userid { get; set; }
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

    if (string.IsNullOrEmpty(par.timestamp))
    {
      return ServiceErrorResult("Pass parameters for timestamp.");
    }
    
    long diff = GetTimeDifferenceInSeconds(par.timestamp);
    if (diff == Settings.TimestampDecodingErrorTag())
    {
      return ServiceErrorResult("Decoding not successful");
    }

    if (string.IsNullOrEmpty(par.userid))
    {
        // Generate a new user id.
        Random rnd = new Random();
        long ts = ((DateTimeOffset)DateTime.UtcNow).ToUnixTimeSeconds();
        long rn = rnd.Next();
        par.userid = $"{ts}-{rn}";
        log.LogInformation($"=> New user {par.userid}");
    }
    else
    {
        log.LogInformation($"=> Existing user {par.userid}");
    }

    try
    {
      var init = new Initialization(par.userid, diff);
      return new OkObjectResult(init);
    }
    catch (Exception ex)
    {
      return ServiceErrorResult($"Request not successful: '{ex.Message}'");
    }
}

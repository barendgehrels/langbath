// RandomImage
// This function is used to call unsplash and return a random image.
// Optionally a subject can be passed. Otherwise it selects a subject from a predefined list.
// (though unsplash also works without any subject).
// It returns a thumb (to display the image immediately at client side, in rough form), an url
// (with the real image), a link (to show in the UI, to credit the author), and some other
// meta information (id, description)
// The first version called unsplash directly from the client. But for authorization purposes,
// and cost and abuse control, it is more convenient to let Azure do it.

#r "Newtonsoft.Json"
#load "..\Common\common.csx"
#load "..\Common\crypt.csx"

using System;
using System.Net;
using System.Text;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Primitives;
using Newtonsoft.Json;

private static class SubjectInfo
{
  public static string[] items
    = new string[]{"landscape", "city", "forest", "mountain", "desert", "village", "road", "world", "beach",
        "tree", "flower", "rose", "grass",
        "orange", "apple", "lemon", "lime",
        "car", "train", "bicycle", "motorbike", "tram", "bus", "ship", "airplane",
        "house", "church", "mosque", "cathedral", "factory", "university", "school", "prison", "palace", "castle", "station",
        "guy", "girl", "man", "woman", "child", "people", "student", "group", "team",
        "party",
        "night", "morning", "evening", "winter", "spring", "summer", "autumn",
        "money", "book", "door",
        "body", "face", "eye", "ear", "hand", "foot", "head", "finger",
        "water", "milk", "beer", "wine",
        "cat", "dog", "cow", "horse", "mouse", "rat", "rabbit", "hamster", "bird", "seagull", "eagle"};
}

private struct ImageInfo
{
    public ImageInfo(string i, string d, string u = null, string k = null, string t = null)
    {
        id = i;
        description = d;
        url = u;
        link = k;
        thumb = t;
    }

    public string id { get; }
    public string description { get; }
    public string url { get; }
    public string link { get; }
    public string thumb { get; }
}

private static async Task<string> MakeBitmapRequest(string uri)
{
    using var client = new HttpClient();
    var response = await client.GetAsync(uri);
    using (var dataStream = await response.Content.ReadAsStreamAsync())
    {
        byte[] bytes = new byte[dataStream.Length];
        dataStream.Read(bytes, 0, (int)dataStream.Length);
        return Convert.ToBase64String(bytes);
    }
}


private static async Task<ImageInfo> MakeRequest(string orientation, string query, ILogger log)
{
    string uri = $"https://api.unsplash.com/photos/random?client_id={Settings.UnsplashClientId()}";
    if (!string.IsNullOrEmpty(query))
    {
        uri += $"&query={query}";
    }
    else
    {
        // Select a random subject
        long index = ((DateTimeOffset)DateTime.UtcNow).ToUnixTimeMilliseconds() % SubjectInfo.items.Length;
        uri += $"&query={SubjectInfo.items[index]}";
    }
    if (!string.IsNullOrEmpty(orientation))
    {
        uri += $"&orientation={orientation}";
    }

    using var client = new HttpClient();
    var response = await client.GetAsync(uri);
    var jsondata = await response.Content.ReadAsStringAsync();

    try
    {
        dynamic json = Newtonsoft.Json.JsonConvert.DeserializeObject(jsondata);

        try
        {
            if (json.ContainsKey("errors"))
            {
                // Propagate the error
                log.LogWarning($"json {jsondata}");
                string msg = json.errors[0];
                return new ImageInfo("", msg);
            }
        }
        catch (Exception ex)
        {
            log.LogError(ex, $"{ex.Message} - json {jsondata}");
        }

        string id = json.id;
        string description = json.description;
        if (string.IsNullOrEmpty(description))
        {
            description = json.alt_description;
        }
        string url = json.urls.regular;
        string link = json.links.html;
        string thumb = json.urls.thumb;
        string bitmap = await MakeBitmapRequest(thumb);
        return new ImageInfo(id, description, url, link, bitmap);
    }
    catch (Exception ex)
    {
        log.LogError(ex, $"{ex.Message} - json {jsondata}");
        return new ImageInfo("", ex.Message);
    }
}

public static OkObjectResult ServiceErrorResult(string message)
{
    return new OkObjectResult(new ErrorNotification($"[RI] {message}"));
}

private struct RequestParameters
{
    public int version { get; set; }
    public string timestamp { get; set; }
    public string query { get; set; }
    public string orientation { get; set; }
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

    if (string.IsNullOrEmpty(par.timestamp))
    {
      return ServiceErrorResult("Pass parameters for timestamp.");
    }

    check = CheckTimeDifference(par.timestamp);
    if (!string.IsNullOrEmpty(check))
    {
      return ServiceErrorResult(check);
    }

    if (!string.IsNullOrEmpty(par.query))
    {
        par.query = GetInputText(par.query, par.version);
        if (par.query == Settings.TextDecodingErrorTag())
        {
            return ServiceErrorResult("Decoding of input text not successful");
        }
    }

    try
    {
        var info = await MakeRequest(par.orientation, par.query, log);
        if (string.IsNullOrEmpty(info.id))
        {
            return ServiceErrorResult(info.description);
        }
        return new OkObjectResult(info);
    }
    catch (Exception ex)
    {
        log.LogError(ex, $"{ex.Message} - query '{par.query}'");
        return ServiceErrorResult(ex.Message);
    }
}

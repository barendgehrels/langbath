// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Implements functionality to call and interpret DeepL
unit lb_deepl_functionality;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lb_describe_picture_settings;

type
  TSentenceAndCorrection = record
    iSentence : string;
    iTranslatedTwice : string;
    iCheckTranslation1 : string;
    iCheckTranslation2 : string;
    iErrorMessage : string;
    iViaLanguage : string;
    iSkip : boolean;
    iLevenshteinDistance : integer;
  end;

// Translate the sentence with DEEPL to all specified VIA-languages, and translate them
// back. The translation closest to the original is kept as the best result.
function TranslateWithDeepL(const settings : TDescribePictureSettings;
  const formality, sentence : string) : TSentenceAndCorrection;


implementation

uses fphttpclient, OpenSslSockets, fpJson,
  lb_lib, lb_lib_string, lb_lib_json;

// Makes a request to DeepL and returns a JSON string
function CallDeepLApi(const ApiUrl, ApiKey : string; const sourceLanguage, targetLanguage,
          formality, input : string) : string;
// The URL should be something like: https://api-free.deepl.com/v2/translate
// The key should be requested at DeepL, it's free (for low volumes)
var
  client : TFPHTTPClient;
  request : string;
  response : TStringStream;
begin
  result := '';
  Response := TStringStream.Create('');
  client := TFPHTTPClient.Create(nil);
  try
    request := 'text=' + input
      + '&source_lang=' + sourceLanguage
      + '&target_lang=' + targetLanguage;
    if (formality <> '') and (targetLanguage = 'RU') then
    begin
      request := request + '&formality=' + formality;
    end;

    // Make the call (the key is never logged)
    client.RequestBody := TRawByteStringStream.Create(request + '&auth_key=' + ApiKey);
    client.AddHeader('Accept', '*/*');
    client.AddHeader('Content-Type','application/x-www-form-urlencoded');
    client.Post(ApiUrl, Response);
    result := Response.DataString;
  finally
    client.free;
    response.free;
  end;

  log(format('CALL DEEPL: %s->%s: %s -> %s', [sourceLanguage, targetLanguage, request, result]));
end;

function GetTranslationFromJson(const Json: string) : string;
var
  jsonData : TJSONData;
  i : integer;
  s : string;
begin
  result := '';

  jsonData := GetJSON(Json);
  try
    i := 0;
    repeat
      s := GetTagAsString(jsonData, format('translations[%d].text', [i]));
      result := result + s;
      inc(i);
    until s = '';
  finally
    jsonData.Free;
  end;
end;


function CallAndGetTranslation(const settings : TDescribePictureSettings; const src, target, formality, sentence : string; out json : string) : string;
begin
  json := CallDeepLApi(settings.iDeepLApiUrl, settings.iDeepLApiKey, src, target, formality, sentence);
  result := GetTranslationFromJson(json);
end;

function ProcessSentence(const settings : TDescribePictureSettings;const formality, sentence : string) : TSentenceAndCorrection;
var
  translatedTwice, via, viaTranslation, jsonAnswer : string;
  bestDistance, levDistance, i : integer;
begin
  Initialize(result);
  result.iSentence := sentence;
  bestDistance := MaxInt;

  // Try to translate using all VIA-languages.
  // Take the translation closest to the original (distance is minimal)
  // If the distance is 0, no more VIA-languages are necessary.
  for i := low(settings.iViaLanguages) to high(settings.iViaLanguages) do
  begin
    via := settings.iViaLanguages[i];
    viaTranslation := CallAndGetTranslation(settings, settings.iTargetLanguage, via, formality, sentence, jsonAnswer);
    if viaTranslation = '' then
    begin
      result.iErrorMessage := concat(result.iErrorMessage, via, ' ', jsonAnswer);
    end
    else
    begin
      translatedTwice := CallAndGetTranslation(settings, via, settings.iTargetLanguage, formality, viaTranslation, jsonAnswer);
      levDistance := LevenshteinDistance(sentence, translatedTwice);

      log(format('PROC DEEPL: %s: dist=%d: %s -> %s -> %s',
          [via, levDistance, sentence, viaTranslation, translatedTwice]));

      if levDistance < bestDistance then
      begin
        result.iTranslatedTwice := translatedTwice;
        result.iViaLanguage := via;
        result.iLevenshteinDistance := levDistance;
        //result.iViaTranslation := viaTranslation;
        bestDistance := levDistance;

        // Save credits, don't call other languages (if any)
        if bestDistance = 0 then exit;

      end else if levDistance = bestDistance then
      begin
        // Both give the same result.
        result.iViaLanguage := result.iViaLanguage + '/' + via;
      end;
    end;
  end;
end;

function TranslateWithDeepL(const settings : TDescribePictureSettings; const formality, sentence : string) : TSentenceAndCorrection;
var jsonIgnored : string;
begin
  result := ProcessSentence(settings, formality, sentence);
  if settings.iCheckLanguage <> '' then
  begin
    result.iCheckTranslation1 := CallAndGetTranslation(settings, settings.iTargetLanguage,
      settings.iCheckLanguage, formality, result.iSentence, jsonIgnored);
    result.iCheckTranslation2 := CallAndGetTranslation(settings, settings.iTargetLanguage,
      settings.iCheckLanguage, formality, result.iTranslatedTwice, jsonIgnored);
  end;
end;

end.


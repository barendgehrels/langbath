// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Unit to detect language errors with online tools (languagetool.org, deepl)

unit lb_detect_language_errors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lb_language_tool_types;

// Makes a request to LanguageTool.org and returns a JSON string
function CallLanguageToolDotOrgAPI(const language, input : string) : string;

function GetCorrectionsFromLanguageTool(const input, json : string) : TLanguageToolCorrection;


implementation

uses fphttpclient, OpenSslSockets, fpjson, jsonparser, lazUtf8, lb_lib, lb_types, lb_lib_json;

function CallLanguageToolDotOrgAPI(const language, input : string) : string;
const url : string = 'https://languagetool.org/api/v2/check';
var
  client : TFPHTTPClient;
  request : string;
  response : TStringStream;
begin
  result := '';
  Response := TStringStream.Create('');
  client := TFPHTTPClient.Create(nil);
  try
    client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    client.AddHeader('Content-Type','application/json; charset=UTF-8');
    client.AddHeader('Accept', 'application/json');
    request := 'text=' + input
        + '&language=' + language
        + '&enabledOnly=false';
    client.RequestBody := TRawByteStringStream.Create(request);
    client.Post(url, Response);
    result := Response.DataString;
  finally
    client.free;
    response.free;
  end;

  log(format('CALL LANGUAGE TOOL: %s -> %s', [request, result]));
end;


function GetCorrectionsFromLanguageTool(const input, json : string) : TLanguageToolCorrection;

  function GetTagAsReplacements(jsonData : TJsonData; const tagBase : string) : TArrayOfString;
  var sub : TJsonData;
    n : integer;
  begin
    result := [];
    SetLength(result, 0);
    n := 0;
    sub := jsonData.FindPath(format('%sreplacements[%d].value', [tagBase, n]));
    while sub <> nil do
    begin
      inc(n);
      SetLength(result, n);
      result[n - 1] := sub.AsString;
      sub := jsonData.FindPath(format('%sreplacements[%d].value', [tagBase, n]));
    end;
  end;

var
  jsonData : TJSONData;
  tag, tagBase : string;
  i, n, offset, len : integer;
begin
  n := 0;
  result.hints := [];
  SetLength(result.hints, 0);

  jsonData := GetJSON(json);

  try
    result.detectedLanguageCode := GetTagAsString(jsonData, 'language.detectedLanguage.code');
    result.detectedLanguage := GetTagAsString(jsonData, 'language.detectedLanguage.name');
    result.detectedLanguageConfidence := GetTagAsDouble(jsonData, 'language.detectedLanguage.confidence');

    i := 0;
    repeat
      tagBase := 'matches[' + inttostr(i) + '].';
      tag := GetTagAsString(jsonData, tagBase + 'sentence');
      if tag <> '' then
      begin
        offset := GetTagAsInteger(jsonData, tagBase + 'offset');
        len := GetTagAsInteger(jsonData, tagBase + 'length');
        if (offset >= 0) and (len > 0) then
        begin
          SetLength(result.hints, n + 1);
          result.hints[n].offset := offset + 1;
          result.hints[n].length := len;
          result.hints[n].replacements := GetTagAsReplacements(jsonData, tagBase);
          result.hints[n].message := GetTagAsString(jsonData, tagBase + 'message');
          result.hints[n].issueType := GetTagAsString(jsonData, tagBase + 'rule.issueType');
          result.hints[n].categoryId := GetTagAsString(jsonData, tagBase + 'rule.category.id');
          result.hints[n].inputPart := UTF8Copy(input, offset + 1, len);
          inc(n);
        end;
      end;
      inc(i);
    until tag = '';
  finally
    jsonData.Free;
  end;
end;

end.


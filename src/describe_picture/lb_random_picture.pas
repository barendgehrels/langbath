// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Functions to retrieve a random picture (from unsplash.com)

unit lb_random_picture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

function RequestUnsplash(const ApiUrl, ApiKey : string; landscape : boolean;
      const subject : string = '') : string;
procedure GetPictureFromJsonAnswer(const Json : string; const picture: TPicture);
procedure PictureLoadFromUrl(const picture: TPicture; const url: String);

implementation

uses
  FpHttpClient, FpJson, JsonParser;


procedure PictureLoadFromUrl(const picture: TPicture; const url: String);
var
  LMemoryStream: TMemoryStream;
begin
  LMemoryStream := TMemoryStream.Create;
  try
    TFPHTTPClient.SimpleGet(url, LMemoryStream);
    LMemoryStream.Position := 0;
    picture.LoadFromStream(LMemoryStream);
  finally
    FreeAndNil(LMemoryStream);
  end;
end;

function RequestUnsplash(const ApiUrl, ApiKey : string; landscape : boolean; const subject : string) : string;
// The URL should be something like: https://api.unsplash.com/photos/random
// The key should be requested at Unsplash, it's free
var
  client : TFPHTTPClient;
  response : TStringStream;
  url : string;
begin
  result := '';
  if ApiUrl = '' then exit;
  url := ApiUrl + '?client_id=' + ApiKey
    + '&orientation=';
  if landscape then url := url + 'landscape' else url := url + 'portrait';

  // Add subject
  if subject <> ''then url := url + '&query=' + subject;

  Response := TStringStream.Create('');
  client := TFPHTTPClient.Create(nil);
  try
    client.Get(url, Response);
    result := Response.DataString;
  finally
    client.free;
    response.free;
  end;
end;

procedure GetPictureFromJsonAnswer(const Json : string; const picture: TPicture);

  function GetTag(jsonData : TJsonData; const tag : string) : string;
  var sub : TJsonData;
  begin
    result := '';
    sub := jsonData.FindPath(tag);
    if sub <> nil then
    begin
      result := sub.AsString;
    end;
  end;

var
  jsonData : TJSONData;
  LMemoryStream: TMemoryStream;
  url : string;
begin
  jsonData := GetJSON(Json);
  if jsonData = nil then exit;

  try
    url := GetTag(jsondata, 'urls.regular');

    // Now get the URL
    LMemoryStream := TMemoryStream.Create;
    try
      TFPHTTPClient.SimpleGet(url, LMemoryStream);
      LMemoryStream.Position := 0;
      picture.LoadFromStream(LMemoryStream);
    finally
      FreeAndNil(LMemoryStream);
    end;

  finally
    jsonData.free;
  end;

end;

end.


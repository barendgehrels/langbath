// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Functions to retrieve a random picture (from unsplash.com)

unit lb_lib_unsplash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TUnsplashRecord = record
    id : string;
    url : string;
    link : string;
    description : string;
    user : string;
  end;

// Makes a request to Unplash and returns a JSON string
function CallUnsplashAPI(const ApiUrl, ApiKey : string; landscape : boolean;
      const subject : string = '') : string;

function GetUnsplashRecordFromJson(const Json : string) : TUnsplashRecord;

// Make a request to Unsplash to get the picture referred to in the JSON string
procedure GetUnsplashPicture(const rec : TUnsplashRecord; const picture: TPicture);

implementation

uses
  FpHttpClient, FpJson, lb_lib, lb_lib_json;

procedure GetPictureFromUrl(const url: String; const picture: TPicture);
var
  LMemoryStream: TMemoryStream;
begin
  log(format('GET PICTURE: %s', [url]));
  LMemoryStream := TMemoryStream.Create;
  try
    TFPHTTPClient.SimpleGet(url, LMemoryStream);
    LMemoryStream.Position := 0;
    picture.LoadFromStream(LMemoryStream);
  finally
    FreeAndNil(LMemoryStream);
  end;
end;

function CallUnsplashAPI(const ApiUrl, ApiKey : string; landscape : boolean; const subject : string) : string;
// The URL should be something like: https://api.unsplash.com/photos/random
// The key should be requested at Unsplash, it's free
var
  client : TFPHTTPClient;
  response : TStringStream;
  request : string;
begin
  result := '';
  if ApiUrl = '' then exit;
  request := ApiUrl + '?orientation=';
  if landscape then request := request + 'landscape' else request := request + 'portrait';

  // Add subject
  if subject <> ''then request := request + '&query=' + subject;

  Response := TStringStream.Create('');
  client := TFPHTTPClient.Create(nil);
  try
    client.Get(request + '&client_id=' + ApiKey, Response);
    result := Response.DataString;
    log(format('CALL UNSPLASH: %s -> %s', [request, result]));
  finally
    client.free;
    response.free;
  end;
end;

function GetUnsplashRecordFromJson(const Json: string): TUnsplashRecord;
var
  jsonData : TJSONData;
begin
  Initialize(result);

  jsonData := GetJSON(Json);
  if jsonData = nil then exit;

  try
    result.id := GetTagAsString(jsondata, 'id');
    result.url := GetTagAsString(jsondata, 'urls.regular');
    result.link := GetTagAsString(jsondata, 'links.html');
    result.description := GetTagAsString(jsondata, 'description');
    result.user := GetTagAsString(jsondata, 'user.name');
    if result.description = '' then
    begin
      result.description := GetTagAsString(jsondata, 'alt_description');
    end;

  finally
    jsonData.free;
  end;
end;

procedure GetUnsplashPicture(const rec : TUnsplashRecord; const picture: TPicture);
begin
  GetPictureFromUrl(rec.url, picture);
end;

end.


unit lb_lib_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJson, JsonParser;

function GetTag(jsonData : TJsonData; const tag : string) : string;
function GetTagAsInteger(jsonData : TJsonData; const tag : string) : integer;
function GetTagAsDouble(jsonData : TJsonData; const tag : string) : double;

implementation

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

function GetTagAsInteger(jsonData : TJsonData; const tag : string) : integer;
var sub : TJsonData;
begin
  result := -1;
  sub := jsonData.FindPath(tag);
  if sub <> nil then
  begin
    result := sub.AsInteger;
  end;
end;

function GetTagAsDouble(jsonData : TJsonData; const tag : string) : double;
var sub : TJsonData;
begin
  result := -1;
  sub := jsonData.FindPath(tag);
  if sub <> nil then
  begin
    result := sub.AsFloat;
  end;
end;


end.


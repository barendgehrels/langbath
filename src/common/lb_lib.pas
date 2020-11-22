// Language Bath - Common
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// This unit provides some general functions

unit lb_lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type TArrayOfString = array of string;

function AssureConfigFolder : boolean;
function SplitTimings(out a, b : double; const s : string) : boolean;
function SplitString(s : string; sep : char) : TArrayOfString;

procedure Log(const s : string);

implementation


function SplitString(s : string; sep : char) : TArrayOfString;
var p, n : integer;
begin
  result := [];
  n := 0;
  p := pos(sep, s);
  while p > 0 do
  begin
    SetLength(result, n + 1);
    result[n] := copy(s, 1, p - 1);
    inc(n);
    delete(s, 1, p);
    p := pos(sep, s);
  end;
  SetLength(result, n + 1);
  result[n] := s;

end;

function SplitTimings(out a, b : double; const s : string) : boolean;
var ar : TArrayOfString;
begin
  ar := SplitString(s, #9);
  if length(ar) >= 2 then
  begin
    result := TryStrToFloat(ar[0], a) and TryStrToFloat(ar[1], b);
  end;
end;

function AssureConfigFolder : boolean;
var folder : string;
begin
  folder := GetAppConfigDir(false);
  result := DirectoryExists(folder);
  if not result then result := CreateDir (folder);
end;

procedure Log(const s : string);
var filename : string;
  txt : TextFile;
  exists : boolean;
begin
  if not AssureConfigFolder then exit;

  filename := GetAppConfigDir(false) + 'langbath.log';
  exists := FileExists(filename);
  AssignFile(txt, filename);
  if exists then Append(txt) else Rewrite(txt);
  WriteLn(txt, s);
  CloseFile(txt);
end;

end.


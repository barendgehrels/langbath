// Language Bath - Common
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// This unit provides some general functions

unit lb_lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type TArrayOfString = array of string;

function ConfigDir : string;
function AssureConfigFolder : boolean;
function SplitTimings(out a, b : double; out r : integer; const s : string) : boolean;
function SplitString(s : string; sep : char) : TArrayOfString;

// NOTE: it's slow.
procedure Log(const s : string);

implementation

var gLogLock: TRTLCriticalSection;

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
  //if s <> '' then
  //begin
  SetLength(result, n + 1);
  result[n] := s;
  //end;
end;

function SplitTimings(out a, b : double; out r : integer; const s : string) : boolean;
var ar : TArrayOfString;
begin
  result := false;

  a := 0;
  b := 0;
  r := 0;

  ar := SplitString(s, #9);

  if length(ar) >= 2 then
  begin
    result := TryStrToFloat(ar[0], a) and TryStrToFloat(ar[1], b);
  end;
  if length(ar) >= 3 then
  begin
    TryStrToInt(ar[2], r);
  end;
end;

function ConfigDir : string;
begin
  // Put all configuration of all programs in the suite
  // in just one place: (windows /users/xxx/AppData/Local/langbath/)
  result := StringReplace(GetAppConfigDir(false), ApplicationName, 'langbath',
         [rfReplaceAll, rfIgnoreCase])
end;

function AssureConfigFolder : boolean;
var folder : string;
begin
  folder := ConfigDir;
  result := DirectoryExists(folder);
  if not result then result := CreateDir (folder);
end;

procedure Log(const s : string);
var filename : string;
  txt : TextFile;
  exists : boolean;
begin
//  writeln(s);
  EnterCriticalSection(gLogLock);
  try

    if not AssureConfigFolder then exit;

    filename := ConfigDir + 'langbath.log';

    exists := FileExists(filename);
    AssignFile(txt, filename);
    if exists then Append(txt) else Rewrite(txt);
    WriteLn(txt, s);
    CloseFile(txt);
  finally
    LeaveCriticalSection(gLogLock);
  end;
end;

initialization
begin
  Initialize(gLogLock);
  InitCriticalSection(gLogLock);
end;

finalization
begin
  DoneCriticalSection(gLogLock);
end;

end.


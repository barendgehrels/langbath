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
function RepairAndSplitString(s : string; sep, quote : char) : TArrayOfString;

// NOTE: it's slow.
procedure Log(const s : string; DoWrite : boolean = false);

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
  // Add last term (regardless if it's empty)
  SetLength(result, n + 1);
  result[n] := s;
end;

function RepairAndSplitString(s : string; sep, quote : char) : TArrayOfString;
var p, pq, n : integer;
  repaired : boolean;
begin
  result := [];
  n := 0;
  p := pos(sep, s);
  while p > 0 do
  begin
    repaired := false;
    SetLength(result, n + 1);
    result[n] := copy(s, 1, p - 1);

    // If it's double quoted, the "sep" might be inside. Try to "repair" this.
    if copy(s, 1, 1) = quote then
    begin
      pq := pos(quote, s, 2);
      if (pq > p) and ((copy(s, pq + 1, 1) = sep) or (pq = length(s))) then
      begin
        p := pq + 1;
        result[n] := stringReplace(copy(s, 1, p - 1), sep, '\t', [rfReplaceAll]);
        log(format('Repair term [%s]', [result[n]]));
        repaired := true;
      end
      else
      begin
        log(format('invalid repair term [%s] %d %d %d', [s, p, pq, length(s)]));
      end;
    end;
    inc(n);
    delete(s, 1, p);
    p := pos(sep, s);
  end;
  // Add last term (also if it's empty - but not if last string was repaired)
  if not repaired then
  begin
    SetLength(result, n + 1);
    result[n] := s;
  end;
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

procedure Log(const s : string; DoWrite : boolean);
var filename : string;
  txt : TextFile;
  exists : boolean;
  hs : THeapStatus;
begin
  hs := GetHeapStatus;

  if doWrite then
  begin
    writeln(inttostr(hs.TotalAllocated) + ' ' + s);
  end;
  EnterCriticalSection(gLogLock);
  try

    if not AssureConfigFolder then exit;

    filename := ConfigDir + 'langbath.log';

    exists := FileExists(filename);
    AssignFile(txt, filename);
    if exists then Append(txt) else Rewrite(txt);
    WriteLn(txt, inttostr(hs.TotalAllocated) + ' ' + s);
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


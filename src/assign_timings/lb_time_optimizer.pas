// Language Bath - Assign Timings
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Code in this unit helps to split two sentences at a pause in the sound volume

unit lb_time_optimizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, lb_bass;

type
  TDetailedLevelInfo = record
    posZeroBegin, posZeroEnd : double;
    baseLevels : array of double;
  end;

procedure AddTimedLevel(var ar : TArrayOfLevel; positionSeconds, aLevel : double; aMax : integer = -1);
function OptimalTime(const ar : TArrayOfLevel; positionSeconds : double) : double;

function GetOptimalTime(out details : TDetailedLevelInfo;
  const ar : TArrayOfLevel; positionSeconds : double) : boolean;

implementation

const
  // Level to detect silences, in units of BASS, which are between 0.0 and 1.0
  KStartLevel = 0.01;
  KMaxSilenceLevel = 0.3; // max level, quite on the high side
  KMaxTimeSeconds = 0.3; // max time to look back or ahead for silence detection


procedure AddTimedLevel(var ar : TArrayOfLevel; positionSeconds, aLevel : double; aMax : integer);
var n : integer;
begin
  n := length(ar);
  SetLength(ar, n + 1);
  ar[n].level := aLevel;
  ar[n].positionSeconds := positionSeconds;
  while (aMax > 0) and (length(ar) > aMax) do
  begin
    delete(ar, 0, 1);
  end;
end;

function ClosestMinimalIndex(const ar : TArrayOfLevel; aPosition, baseLevel : double) : integer;
var i : integer;
  timeDiff, minTimeDiff : double;
begin
  result := -1;
  minTimeDiff := MaxDouble;
  for i := low(ar) to high(ar) do
  begin
    timeDiff := abs(aPosition - ar[i].positionSeconds);
    if (timeDiff < KMaxTimeSeconds) and (ar[i].level <= baseLevel) and (timeDiff < minTimeDiff) then
    begin
      minTimeDiff := timeDiff;
      result := i;
    end;
  end;
end;

procedure GetClosestZeroIndex(out index : integer; out baseLevel : double;
  var details : TDetailedLevelInfo;
  const ar : TArrayOfLevel; aPosition : double);
var n : integer;
begin
  baseLevel := KStartLevel;

  details.baseLevels := [];
  SetLength(details.baseLevels, 1);
  details.baseLevels[0] := baseLevel;
  n := 1;

  repeat
    index := ClosestMinimalIndex(ar, aPosition, baseLevel);

    if index < 0 then
    begin
      baseLevel := (baseLevel + KStartLevel) * 1.075;
      //baseLevel := baseLevel * 1.1;
      SetLength(details.baseLevels, n + 1);
      details.baseLevels[n] := baseLevel;
      inc(n);
    end;
  until (index >= 0) or (baseLevel > KMaxSilenceLevel);
end;

procedure GetZeroPeriod(out b, e : integer;
  minLevel : double; const ar : TArrayOfLevel; index : integer);
var i : integer;
  c : boolean;
begin
  b := index;
  e := index;
  if length(ar) <= 1 then
  begin
    exit;
  end;

  c := true;
  for i := index downto low(ar) do
  begin
    if c and (ar[i].level <= minLevel) then b := i else c := false;
  end;
  c := true;
  for i := index to high(ar) do
  begin
    if c and (ar[i].level <= minLevel) then e := i else c := false;
  end;
end;

function GetOptimalTime(
  out details : TDetailedLevelInfo;
  const ar : TArrayOfLevel; positionSeconds : double) : boolean;
var z, e, b : integer;
  minLevel : double;
begin
  result := false;
  details.posZeroBegin := positionSeconds;
  details.posZeroEnd := positionSeconds;

  GetClosestZeroIndex(z, minLevel, details, ar, positionSeconds);
  if z >= 0 then
  begin
    GetZeroPeriod(b, e, minLevel, ar, z);
    Details.posZeroBegin := ar[b].positionSeconds;
    details.posZeroEnd := ar[e].positionSeconds;
    result := true;
  end;
end;

function OptimalTime(const ar : TArrayOfLevel; positionSeconds : double) : double;
var details : TDetailedLevelInfo;
begin
  result := positionSeconds;
  details.baseLevels := [];
  if GetOptimalTime(details, ar, positionSeconds) then
  begin
    result := (details.posZeroBegin + details.posZeroEnd) / 2.0;
  end
  else
  begin
    result := positionSeconds;
  end;
end;

end.


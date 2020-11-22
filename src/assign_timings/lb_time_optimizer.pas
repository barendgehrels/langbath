// Language Bath - Assign Timings
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Code in this unit helps to split two sentences at a pause in the sound volume

unit lb_time_optimizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTimedLevel = record
    level : double;
    positionSeconds : double;
  end;

  TTimedLevelArray = array of TTimedLevel;

procedure AddTimedLevel(var ar : TTimedLevelArray; aLevel, positionSeconds : double; aMax : integer);
function TimedLevelToString(const ar : TTimedLevelArray; positionSeconds : double) : string;
function OptimalTime(const ar : TTimedLevelArray; positionSeconds : double) : double;


implementation

const KMinLevel = 0.15; // should be substantial, breathing is already 0.07

procedure AddTimedLevel(var ar : TTimedLevelArray; aLevel, positionSeconds : double; aMax : integer);
var n : integer;
begin
  n := length(ar);
  SetLength(ar, n + 1);
  ar[n].level := aLevel;
  ar[n].positionSeconds := positionSeconds;
  while length(ar) > aMax do
  begin
    delete(ar, 0, 1);
  end;
end;

function ClosestMinimalIndex(const ar : TTimedLevelArray; aPosition, baseLevel : double) : integer;
var i : integer;
  d, minD : double;
begin
  result := -1;
  minD := 1.0e10;
  for i := low(ar) to high(ar) do
  begin
    d := abs(aPosition - ar[i].positionSeconds);
    if (ar[i].level <= baseLevel) and (d < minD) then
    begin
      minD := d;
      result := i;
    end;
  end;
end;

procedure GetClosestZeroIndex(out index : integer; out baseLevel : double;
  const ar : TTimedLevelArray; aPosition : double);
begin
  baseLevel := KMinLevel;
  repeat
    index := ClosestMinimalIndex(ar, aPosition, baseLevel);
    if index < 0 then baseLevel := baseLevel * 1.1;
  until (index >= 0) or (baseLevel > 0.6);
end;

procedure GetZeroPeriod(out b, e : integer;
  minLevel : double; const ar : TTimedLevelArray; index : integer);
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

function OptimalTime(const ar : TTimedLevelArray; positionSeconds : double) : double;
var z, e, b : integer;
  minLevel : double;
begin
  result := positionSeconds;

  GetClosestZeroIndex(z, minLevel, ar, positionSeconds);
  if z >= 0 then
  begin
    GetZeroPeriod(b, e, minLevel, ar, z);
    result := (ar[b].positionSeconds + ar[e].positionSeconds) / 2.0;
  end;
end;

function TimedLevelToString(const ar : TTimedLevelArray; positionSeconds : double) : string;
var i, z, e, b : integer;
  t, minLevel : double;

begin
  result := '';
  GetClosestZeroIndex(z, minLevel, ar, positionSeconds);
  GetZeroPeriod(b, e, minLevel, ar, z);
  for i := low(ar) to high(ar) do
  begin
    if i = b then result := result + ' [';

    result := result + ' ' + format('%.2f', [ar[i].level]);

    if i = z then result := result + '!'; // Closest zero

    if (i + 1 <= high(ar))
    and (ar[i].positionSeconds <= positionSeconds)
    and (ar[i + 1].positionSeconds >= positionSeconds)
    then
    begin
      result := result + '*'; // Selected timestamp
    end;

    if i = e then result := result + ' ]';
  end;

  if length(ar) > 0 then
  begin
    t := OptimalTime(ar, positionSeconds);
    result := result
    + format(' {%.4f .. %.4f}',
      [ar[0].positionSeconds - positionSeconds, ar[high(ar)].positionSeconds - positionSeconds]);

    result := result + format(' zero=%.3f', [ar[e].positionSeconds - ar[b].positionseconds]);
    result := result + format(' opt=%.3f', [t]);
    result := result + format(' minlev=%.2f', [minLevel]);
    result := result + format(' prec=%d', [trunc(1000*(positionSeconds - t))]);
  end;
end;

end.


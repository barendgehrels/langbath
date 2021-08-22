// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Small class containing a frequency counter
// The goal is to report things like:
//   To understand 70 %      : 520 words
//   To understand 80 %      : 1810 words
//   To understand 90 %      : 4640 words
// Which means that if you know 4700 words, you understand 90% of all the words in the text
// For this, pass the stepsize in the constructor and call ReportFrom
//
// Or, the other way round, report things like:
//   Words top  2500         : 83 %
//   Words top  5000         : 90 %
//   Words top  7500         : 93 %
//   Words top 10000         : 95 %
// Which also means that if you know 7500 words, you understand 95% of all the words.
// For this, pass the desired limits in the constructor and call ReportAll

unit lb_frequency_counter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TFrequencyCounter = class
  public
    constructor Create(stepSize, minRank, maxRank : integer);
    constructor Create(const rankLimits : array of integer);
    procedure Add(rank : integer);
    procedure ReportFrom(target, interval : double);
    procedure ReportAll;
  private
    countRank : integer;
    countBelow : array of integer;
    limits : array of integer;
  end;

implementation

{ TFrequencyCounter }

constructor TFrequencyCounter.Create(stepSize, minRank, maxRank : integer);
var i, n, rank : integer;
begin
  countRank := 0;

  SetLength(limits, 1);
  rank := minRank;
  limits[0] := minRank;
  n := 1;
  repeat
    inc(rank, stepSize);
    inc(n);
    SetLength(limits, n);
    limits[n - 1] := rank;
  until rank >= maxRank;

  SetLength(countBelow, length(limits));
  for i := low(countBelow) to high(countBelow) do countBelow[i] := 0;
end;

constructor TFrequencyCounter.Create(const rankLimits : array of integer);
var i : integer;
begin
  countRank := 0;
  SetLength(limits, length(rankLimits));
  SetLength(countBelow, length(limits));
  for i := low(rankLimits) to high(rankLimits) do
  begin
    limits[i] := rankLimits[i];
    countBelow[i] := 0;
  end;
end;

procedure TFrequencyCounter.Add(rank: integer);
var i : integer;
begin
  inc(countrank);
  for i := low(limits) to high(limits) do
  begin
    if rank <= limits[i] then inc(countBelow[i]);
  end;
end;

procedure TFrequencyCounter.ReportFrom(target, interval : double);
var i : integer;
  percentage : double;
begin
  for i := low(limits) to high(limits) do
  begin
    percentage := 100.0 * countBelow[i] / countrank;
    if percentage > target then
    begin
      writeln(format('To understand %d %%      : %d words', [trunc(percentage), limits[i]]));
      target := target + interval;
    end;
  end;
end;

procedure TFrequencyCounter.ReportAll;
var i : integer;
  percentage : double;
begin
  for i := low(limits) to high(limits) do
  begin
    percentage := 100.0 * countBelow[i] / countrank;
    writeln(format('Words top %5d         : %d %%', [limits[i], trunc(percentage)]));
  end;
end;

end.


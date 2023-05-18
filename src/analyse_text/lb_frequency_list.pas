// Language Bath
// Copyright (c) 2022 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Reads a frequency list, and related functions.
// Expected format:
// - no header
// - two entries per line, FREQUENCY and WORD
// - can be separated by TAB, space or comma

unit lb_frequency_list;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFrequencyEntry = record
    rank : integer;
    line : integer;
    source : integer;
    count : integer;
    entry : string;
    mainEntry : string;
  end;

  TArrayOfFrequencyEntry = array of TFrequencyEntry;

procedure AppendList(var f1 : TArrayOfFrequencyEntry; const f2 : TArrayOfFrequencyEntry);
function ReadFrequencyList(const frequencyFilename : string; verbose : boolean = false) : TArrayOfFrequencyEntry;
function GetOtherFrequencyList(const filename : string; rank, source : integer) : TArrayOfFrequencyEntry;

procedure SortByEntryAndRank(var a : TArrayOfFrequencyEntry);
function FindByEntry(const a : array of TFrequencyEntry; const entry : string) : integer;


implementation

uses LazUtf8, lb_lib, lb_types, lb_quicksort, lb_lib_string;

type
  TCompareByEntry = class
    function Less(const a, b : TFrequencyEntry) : boolean;
  end;
  TCompareByEntryAndRank = class
    function Less(const a, b : TFrequencyEntry) : boolean;
  end;

function TCompareByEntry.Less(const a, b : TFrequencyEntry) : boolean;
begin
  result := a.entry < b.entry
end;

function TCompareByEntryAndRank.Less(const a, b : TFrequencyEntry) : boolean;
begin
  result := (a.entry < b.entry) or ((a.entry = b.entry) and (a.rank < b.rank));
end;

procedure SortByEntryAndRank(var a : TArrayOfFrequencyEntry);
var b : TArrayOfFrequencyEntry;
  i, n : integer;
begin
  specialize CallQuickSort<TFrequencyEntry, TCompareByEntryAndRank>(a);

  // Remove all cases with duplicate entry (this will keep the lowest ranks because it's sorted)
  b := [];
  n := 0;
  for i := low(a) to high(a) do
  begin
    if (n = 0) or (a[i].entry <> b[n - 1].entry) then
    begin
      SetLength(b, n + 1);
      b[n] := a[i];
      inc(n);
    end;
  end;
  a := b;
end;

function FindByEntry(const a : array of TFrequencyEntry; const entry : string) : integer;
var r : TFrequencyEntry;
begin
  r.entry := ReplaceEquivalents(entry);
  result := specialize CallFind<TFrequencyEntry, TCompareByEntry>(a, r);
end;

procedure AddEntry(rank, source : integer; entry : string; const mainEntry : string;
  var entries : TArrayOfFrequencyEntry; var index : cardinal);
begin
  entry := trim(UTF8LowerString(entry));
  // TEMP (currently input sometimes contains it)
  entry := StringReplace(entry, '*', '', [rfReplaceAll]);
  if entry = '' then exit;

  if length(entries) = index then
  begin
    // Increase it's capacity
    SetLength(entries, length(entries) * 2);
  end;

  entries[index].source := source;
  entries[index].rank := rank;
  entries[index].entry := entry;
  entries[index].mainEntry := mainEntry;
  inc(index);

  // Replace ё with е and remove - (e.g. in кто-то), both in list and in words themselves
  // to find them and avoid taking various places of dashes into account
  // These entries are added in duplicate
  if HasEquivalents(entry) then
  begin
    AddEntry(rank, source, ReplaceEquivalents(entry), mainEntry, entries, index);
  end;
  if UTF8Pos('-', entry) > 0 then
  begin
    entry := UTF8StringReplace(entry, '-', '', [rfReplaceAll]);
    AddEntry(rank, source, entry, mainEntry, entries, index);
  end;
end;

procedure AppendList(var f1: TArrayOfFrequencyEntry;
  const f2: TArrayOfFrequencyEntry);
var i, n1, n2 : integer;
begin
  n1 := length(f1);
  n2 := length(f2);
  SetLength(f1, n1 + n2);
  for i := low(f2) to high(f2) do
  begin
    f1[n1 + i] := f2[i];
  end;
end;

function ReadFrequencyList(const frequencyFilename : string; verbose : boolean) : TArrayOfFrequencyEntry;
var list : TStringList;
  rank, i : integer;
  resultIndex : cardinal;
  written_term1 : string;
  ar : TArrayOfString;
begin
  result := [];
  if frequencyFileName = '' then exit;

  if verbose then writeln('Reading frequency list ' + frequencyFilename + '...');

  written_term1 := '';
  list := TStringList.Create;
  try
    list.LoadFromFile(frequencyFilename);

    // Reserve space for the list, plus 10% for some variations,
    // and set the result index (= result count)
    resultIndex := 0;
    SetLength(result, round(list.count * 1.1));

    for i := 0 to list.Count - 1 do
    begin
      ar := SplitString(list[i], #9);
      if length(ar) = 1 then
      begin
        ar := SplitString(list[i], ' ');
      end;
      if length(ar) = 1 then
      begin
        ar := SplitString(list[i], ',');
      end;
      if length(ar) = 2 then
      begin
        SetLength(ar, 3);
      end;
      if length(ar) = 3 then
      begin
        rank := strtoint(ar[0]);

        if ar[2] <> '' then
        begin
          // Write second term
          AddEntry(rank, 1, ar[2], ar[1], result, resultIndex);

          if (ar[1] <> ar[2]) and (ar[1] <> written_term1) then
          begin
            // It differs and is not yet written (it's sorted on term1 too).
            AddEntry(rank, 1, ar[1], ar[1], result, resultIndex);
            written_term1 := ar[1];
          end;
        end
        else
        begin
          // There is no second term, add first
          AddEntry(rank, 1, ar[1], ar[1], result, resultIndex);
        end;

      end else writeln('Incomplete ' + IntToStr(length(ar)) + ' ' + frequencyFilename + ' ' + ArrayAsString(ar));
    end;

  finally
    SetLength(result, resultIndex);
    if verbose then
    begin
      writeln(format('Finish reading frequency list %d entries from %d lines',
        [length(result), list.count]));
    end;
    list.free;
  end;
end;

function GetOtherFrequencyList(const filename : string; rank, source : integer) : TArrayOfFrequencyEntry;
var
  list : TStringList;
  i : integer;
  resultIndex : cardinal;
begin
  result := [];
  if filename <> '' then
  begin
    list := TStringList.Create;
    try
      list.LoadFromFile(filename);
      // Set Length plus 10% extra
      SetLength(result, round(list.count * 1.1));
      resultIndex := 0;
      for i := 0 to list.count - 1 do
      begin
        AddEntry(rank, source, list[i], list[i], result, resultIndex);
      end;
      SetLength(result, resultIndex);

    finally
      list.free;
    end;
  end;
end;

end.


// Language Bath
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// The code unit splits a textfile with strings into another textfile with split sentences

unit lb_split_textfile_into_sentences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure SplitSentences(const inputFilename, outputFilename, honorificsFilename : string; analyze: boolean);

implementation

uses LazUtf8, lb_split_string_into_sentences, lb_lib;

// Returns true if the sentence is a roman numeral
function IsRomanNumeral(const s : string) : boolean;

  function IsNumeral(c : char) : boolean;
  const numerals : array of char = ('I', 'V', 'X', 'L', 'C', 'D', 'M');
  var i : integer;
  begin
    result := false;
    for i := low(numerals) to high(numerals) do
    begin
      if c = numerals[i] then begin result := true; exit; end;
    end;
  end;

var i : integer;
begin
  result := false;
  if length(s) = 0 then exit;

  // Walk the UTF string as if it were a normal string
  for i := 1 to length(s) do
  begin
    if not IsNumeral(s[i]) then exit;
  end;
  result := true;
end;

// Remove notes in a string, specified for example as [1234]
function RemoveNotes(const s : string) : string;
var i, p, len, sublen, note : integer;
begin
  result := s;

  // Walk the UTF string as if it were a normal string
  p := 0;
  i := 1;
  len := length(result);
  while i <= len do
  begin
    if result[i] = '[' then
    begin
      p := i;
    end else if result[i] = ']' then
    begin
      sublen := 1 + i - p;
      if sublen < 10  then
      begin
        if TryStrToInt(copy(result, p + 1, sublen - 2), note) then
        begin
          delete(result, p, sublen);
          dec(len, sublen);
        end;
        p := 0;
      end;
    end;
    inc(i);
  end;
end;

function CleanText(list : TStringList) : string;
var s : string;
  k : integer;
begin
  // Process it per line:
  // - Adapt all lines with only a Roman numeral, because this is usually a chapter number.
  //   Make it a sentence and denote it as special by a #
  // - Remove notes (e.g. [1234]) sometimes occuring in some old literature
  for k := 0 to list.Count - 1 do
  begin
    list[k] := trim(list[k]);
    if IsRomanNumeral(list[k]) then list[k] := '# ' + list[k] + '.';
    if list[k].StartsWith('#') and not list[k].EndsWith('.') then
    begin
      list[k] := list[k] + '.';
    end;
    list[k] := RemoveNotes(list[k]);
  end;

  // Process the whole
  s := list.text;

  // Change line breaks and form feeds into spaces
  s := StringReplace(s, #10, ' ', [rfReplaceAll]);
  s := StringReplace(s, #12, ' ', [rfReplaceAll]);
  s := StringReplace(s, #13, ' ', [rfReplaceAll]);

  // Correct ellipsis, we want dots here
  s := StringReplace(s, '…', '...', [rfReplaceAll]);

  // Remove duplicate spaces
  repeat
    s := StringReplace(s, '  ', ' ', [rfReplaceAll]);
  until pos('  ', s) = 0;

  result := s;
end;

procedure SplitLongSentence(var k : integer; list : TStringList;
   minLengthToSplit, minLengthToKeep : word);
var p, len : integer;
  s1, s2 : string;
begin
  len := UTF8Length(list[k]);
  if len >= minLengthToSplit then
  begin
    // The string is long enough. Find first separator, but skip first part
    p := UTF8Pos(';', list[k], minLengthToKeep);
    if (p >= minLengthToKeep) and (len - p >= minLengthToKeep) then
    begin
      // Parts left and right of [p] are long enough
      s1 := UTF8Copy(list[k], 1, p);
      s2 := UTF8Copy(list[k], p + 1, len - p);
      list[k] := s1;
      inc(k);
      list.Insert(k, trim(s2));

      // Recursively try to split the remaining part
      SplitLongSentence(k, list, minLengthToSplit, minLengthToKeep);
    end;
  end;
end;

procedure SplitLongSentences(list : TStringList; minLengthToSplit, minLengthToKeep : word);
var i : integer;
begin
  i := 0;
  while i < list.Count do
  begin
    if pos(';', list[i]) > 0 then
    begin
      SplitLongSentence(i, list, minLengthToSplit, minLengthToKeep);
    end;
    inc(i);
  end;
end;

function ReadHonorifics(const honorificsFilename : string) : THonorificArray;
var list : TStringList;
  ar : array of string;
  i, count : integer;
begin
  result := [];
  list := TStringList.Create;
  try
    list.LoadFromFile(honorificsFilename);
    for i := 0 to list.Count - 1 do
    begin
      ar := SplitString(list[i], #9);
      if length(ar) = 1 then ar := SplitString(list[i], ',');
      if length(ar) >= 2 then
      begin
        // Also read the count, if specified, though it is currently not used at all
        count := 1;
        if length(ar) >= 3 then TryStrToInt(ar[2], count);

        AddToMap(result, ar[0], ar[1], count);
      end;
    end;

  finally
    list.free;
  end;
end;

procedure SplitSentences(const inputFilename, outputFilename, honorificsFilename : string; analyze: boolean);
var inputList, outputList : TStringList;
  honorifics : THonorificArray;
  allText : string;
  i : integer;
begin
  if not FileExists(inputFilename) or FileExists(outputFilename) then
  begin
    exit;
  end;

  honorifics := [];

  inputList := TStringList.Create;
  try
    inputList.LoadFromFile(inputFileName);
    allText := CleanText(inputList);
  finally
    inputList.Free;
  end;

  if analyze then
  begin
    honorifics := GetHonorifics(allText);
    outputList := TStringList.Create;
    for i := low(honorifics) to high(honorifics) do
    begin
      outputList.Append(honorifics[i].honorific + ',' + honorifics[i].name + ',' + inttostr(honorifics[i].count));
    end;
    outputList.SaveToFile(outputFilename);
    exit;
  end;

  if honorificsFilename <> '' then
  begin
    honorifics := ReadHonorifics(honorificsFilename);
  end;

  outputList := SplitStringIntoSentences(allText, honorifics);
  try
    // Postprocess the split sentences: try to split long strings at semicolons
    SplitLongSentences(outputList, 60, 25);

    outputList.SaveToFile(outputFilename);
  finally
    outputList.free;
  end;
end;

end.


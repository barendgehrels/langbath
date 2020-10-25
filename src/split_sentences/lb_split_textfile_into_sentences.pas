// Language Bath
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// The code unit splits a string into multiple sentences.
// Original use case is:
//   - the user starts with a PDF, containing some text (for example a novel),
//   - save its content into a textfile (for example with Acrobat Reader).
//   - it can have Latin or Cyrillic/Russian characters
//   - optionally perform minor manual cleanup (for example remove references)
//   - split it into sentences:
//       - separated by full stops
//       - long sentences can be splitted at semi colons too
//   - because these sentences should act as examples for words in the
//     language the user is learning.
//   - they should preferably be small, but that is not always possible
//     especially in Russian novels.

unit lb_split_textfile_into_sentences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure SplitSentences(const inputFilename, outputFilename : string);
procedure RemoveNotes(var s : string);

implementation

uses LazUtf8;

// Classifies a code point (of one UTF character) as
// Uppercase (1), Lowercase (2) or else (3)
function Classify(const codePoint : string; resultIfNumber : integer) : integer;
var lower, upper : string;
begin
  result := 3;

  // Spaces are most common in calling this function, return immediately
  if codePoint = ' ' then exit;

  if (length(codePoint) = 1) and (codePoint[1] >= '0') and (codePoint[1] <= '9') then
  begin
    result := resultIfNumber;
    exit;
  end;

  // Check UTF8 multi byte character
  lower := UTF8LowerString(codePoint);
  upper := UTF8UpperString(codePoint);

  if lower <> upper then
  begin
    // It is a character
    if codePoint = upper then result := 1
    else if codePoint = lower then result := 2;
  end;
end;

// Sentences are splitted by: full stop, question mark, exclamation mark
function IsPossibleSplit(const codePoint : string) : boolean;
begin
  result := (CodePoint = '.') or (CodePoint = '?') or (CodePoint = '!');
end;

procedure ExtractStartOfSentence(const s : string;
   var start, finish : string);
var
  i, len, code : integer;
  codePoint : string;
begin
  start := s;
  finish := '';

  len := UTF8Length(s);
  for i := len downto 1 do
  begin
    codePoint := UTF8Copy(s, i, 1);
    code := classify(codePoint, 2);
    if IsPossibleSplit(codePoint)
    or (codePoint = '»')
    or (codePoint = '"')
    or (codePoint = '''')
    or (codePoint = ')')
    or (codePoint = '}')
    or (codePoint = ']') // in notes
    or (codePoint = '>') // in notes
    or (codePoint = ';')
    or (code in [1,2]) then
    begin
     start := UTF8Copy(s, 1, i);
     finish := UTF8Copy(s, i + 1, len - i);
     exit;
    end;
  end;
end;

procedure RemoveNotes(var s : string);
var i, p, len, sublen : integer;
begin
  // Walk the UTF string as if it were a normal string
  p := 0;
  i := 1;
  len := length(s);
  while i <= len do
  begin
    if s[i] = '[' then
    begin
      p := i;
    end else if s[i] = ']' then
    begin
      sublen := 1 + i - p;
      if sublen < 10 then
      begin
        delete(s, p, sublen);
        dec(len, sublen);
        p := 0;
      end;
    end;
    inc(i);
  end;
end;

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

  // It's not necessary here to walk through UTF codepoints, if it's e.g. Russian it will
  // return false immediately.
  for i := 1 to length(s) do
  begin
    if not IsNumeral(s[i]) then exit;
  end;
  result := true;
end;


function CleanString(list : TStringList) : string;
var s, item : string;
  k : integer;
begin
  // Adapt all lines with only a Roman numeral, because this is usually a chapter.
  // Make it a sentence and denote it as special by a #
  for k := 0 to list.Count - 1 do
  begin
    item := trim(list[k]);
    if IsRomanNumeral(item) then list[k] := '# ' + item + '.';
  end;

  s := list.text;

  // Change line breaks and form feeds into spaces
  s := StringReplace(s, #10, ' ' , [rfReplaceAll]);
  s := StringReplace(s, #12, ' ', [rfReplaceAll]);
  s := StringReplace(s, #13, ' ', [rfReplaceAll]);

  // Correct ellipsis, we want dots here
  s := StringReplace(s, '…', '...', [rfReplaceAll]);

  // Remove duplicate spaces
  repeat
    s := StringReplace(s, '  ', ' ', [rfReplaceAll]);
  until pos('  ', s) = 0;

  RemoveNotes(s);

  result := s;
end;

procedure AppendCleanOutput(list : TStringList; s : string);
begin
  s := trim(s);
  if length(s) > 0 then
  begin
    list.Append(s);
  end;
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

procedure SplitSentences(const inputFilename, outputFilename : string);
var inputList, outputList : TStringList;
  s : string;
  currentPosition, endPosition: PChar;
  code, len: Integer;
  codePoint: String;

  next, sentence, outputString : string;
  isPossibleEndOfSentence : boolean;

begin
  if not FileExists(inputFilename) or FileExists(outputFilename) then
  begin
    exit;
  end;

  outputString := '';
  isPossibleEndOfSentence := false;

  inputList := TStringList.Create;
  outputList := TStringList.Create;

  try
    inputList.LoadFromFile(inputFileName);
    s := CleanString(inputList);

    // Walk through this inputList
    codePoint := '';
    currentPosition := PChar(s);
    endPosition := currentPosition + length(s);
    while currentPosition < endPosition do
    begin
      len := UTF8CodepointSize(currentPosition);
      SetLength(codePoint, len);
      Move(currentPosition^, codePoint[1], len);

      if isPossibleEndOfSentence then
      begin
        code := Classify(codePoint, 1);
        if code = 1 then
        begin
          if (UTF8Length(outputString) > 30)
          or (pos(' ', trim(outputString)) > 0) then
          begin
            // New sentence, and last sentence was large enough
            // (short exclamations like 'Вы...', 'почему?', 'Да!'
            //  are combined with next sentence)
            sentence := '';
            next := '';
            ExtractStartOfSentence(outputString, sentence, next);
            AppendCleanOutput(outputList, sentence);
            outputString := next;
          end;
          isPossibleEndOfSentence := false;
        end
        else if code = 2 then
        begin
          isPossibleEndOfSentence := false;
        end;
      end;

      outputString := outputString + codePoint;

      // At a full stop (.) or other (?, !): if next character is a space,
      // and next-next (or further) character is a capital,
      // then the sentence is considered as complete.
      if IsPossibleSplit(codePoint) then
      begin
        isPossibleEndOfSentence := true;
      end;

      inc(currentPosition, len);
    end;

    // Add the last string, if any
    AppendCleanOutput(outputList, outputString);

    // Try to split long strings at semicolons
    SplitLongSentences(outputList, 60, 25);

    outputList.SaveToFile(outputFilename);

  finally
    inputList.Free;
    outputList.free;
  end;
end;

end.


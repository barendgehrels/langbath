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

const
  RussianLowerCase : array of string =
    ('а', 'б', 'в', 'г', 'д', 'е', 'ё', 'ж', 'з', 'и', 'й',
     'к', 'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф',
     'х', 'ц', 'ч', 'ш', 'щ', 'ъ', 'ы', 'ь', 'э', 'ю', 'я');
  RussianUpperCase : array of string =
    ('А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ё', 'Ж', 'З', 'И', 'Й',
     'К', 'Л', 'М', 'Н', 'О', 'П', 'Р', 'С', 'Т', 'У', 'Ф',
     'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ы', 'Ь', 'Э', 'Ю', 'Я');

// Classifies a string (of one UTF character) as
// Uppercase (1), Lowercase (2) or else (3)
function Classify(const s : string; resultIfNumber : integer) : integer;
var i : integer;
begin
  result := 3;

  // Most common cases: spaces, ...
  if s = ' ' then exit;

  if length(s) = 1 then
  begin
   // 1 byte. Assume ASCII (Western) and check.
   if (s[1] >= 'A') and (s[1] <= 'Z') then
   begin
     result := 1;
     exit;
   end;
   if (s[1] >= 'a') and (s[1] <= 'z') then
   begin
     result := 2;
     exit;
   end;
   if (s[1] >= '0') and (s[1] <= '9') then
   begin
     result := resultIfNumber;
     exit;
   end;
  end;

  // Check Russian sentences
  result := 0;
  assert(length(RussianLowerCase) = length(RussianUpperCase));
  for i := low(RussianUpperCase) to high(RussianUpperCase) do
  begin
    if RussianUpperCase[i] = s then
    begin
      result := 1;
      exit;
    end;
    if RussianLowerCase[i] = s then
    begin
       result := 2;
       exit;
    end;
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

function CleanString(const t : string) : string;
var s : string;
begin
  s := t;

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

function IsRoman(const s : string) : boolean;
var i : integer;
begin
  // TODO: this can go wrong with an English sentence
  // starting e.g. with "I will"
  result := false;
  for i := 1 to length(s) do
  begin
    if (s[i] <> 'I')
    and (s[i] <> 'V')
    and (s[i] <> 'X')
    then
    begin
      exit;
    end;
  end;
  result := true;
end;

procedure AppendCleanOutput(list : TStringList; s : string);
var p : integer;
begin
  s := trim(s);
  p := pos(' ', s);
  if (p > 0) and (p < 5) then
  begin
    if IsRoman(copy(s, 1, p - 1)) then
    begin
      delete(s, 1, p);
    end;
  end;
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
  inputList := TStringList.Create;
  outputList := TStringList.Create;

  outputString := '';
  isPossibleEndOfSentence := false;

  try
    inputList.LoadFromFile(inputFileName);
    s := CleanString(inputList.text);

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


// Language Bath
// Copyright (c) 2020, 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// The code unit splits one (usually big) string into multiple sentences.
// Original use case is:
//   - the user starts with a PDF, containing some text (for example a novel),
//   - save its content into a textfile (for example with Acrobat Reader).
//   - it can have Latin, Cyrillic/Russian or other UTF8 characters
//   - optionally perform minor manual cleanup (for example remove references)
//   - split it into sentences:
//       - separated by full stops
//       - long sentences can be splitted at semi colons too
//   - because these sentences should act as examples for words in the
//     language the user is learning.
//   - sentences should preferably be small, but that is not always possible
// The result won't be perfect but it can be postprocessed.

unit lb_split_string_into_sentences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function SplitStringIntoSentences(const sentenceCollection : string) : TStringList;

implementation

uses LazUtf8;

type
  TCodePointClassification = (CPCUpper, CPCLower, CPCOther);

// Classifies a code point (of one UTF character) as upper, lower or other
function Classify(const codePoint : string;
     resultIfNumber : TCodePointClassification) : TCodePointClassification;
var lower, upper : string;
begin
  result := CPCOther;

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
    // Trick: if lower case is not upper case, it is a character
    // (this trick is valid for Latin/Cyrillic texts)
    if codePoint = upper then result := CPCUpper
    else if codePoint = lower then result := CPCLower;
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
  i, len : integer;
  codePoint : string;
begin
  start := s;
  finish := '';

  len := UTF8Length(s);
  for i := len downto 1 do
  begin
    codePoint := UTF8Copy(s, i, 1);
    if IsPossibleSplit(codePoint)
    or (codePoint = '»')
    or (codePoint = '"')
    or (codePoint = '''')
    or (codePoint = ')')
    or (codePoint = '}')
    or (codePoint = ']') // in notes
    or (codePoint = '>') // in notes
    or (codePoint = ';')
    or (Classify(codePoint, CPCLower) <> CPCOther) then
    begin
     start := UTF8Copy(s, 1, i);
     finish := UTF8Copy(s, i + 1, len - i);
     exit;
    end;
  end;
end;

procedure AppendCleanOutput(list : TStringList; s : string);
begin
  s := trim(s);
  if length(s) > 0 then
  begin
    list.Append(s);
  end;
end;

function SplitStringIntoSentences(const sentenceCollection : string) : TStringList;
var
  currentPosition, endPosition: PChar;
  len: Integer;
  code : TCodePointClassification;
  codePoint: String;

  next, sentence, outputString : string;
  isPossibleEndOfSentence : boolean;

begin
  outputString := '';
  isPossibleEndOfSentence := false;

  result := TStringList.Create;

  codePoint := '';
  currentPosition := PChar(sentenceCollection);
  endPosition := currentPosition + length(sentenceCollection);
  while currentPosition < endPosition do
  begin
    len := UTF8CodepointSize(currentPosition);
    SetLength(codePoint, len);
    Move(currentPosition^, codePoint[1], len);

    if isPossibleEndOfSentence then
    begin
      code := Classify(codePoint, CPCUpper);
      if code = CPCUpper then
      begin
        // New sentence detected. Append the current one and start again
        sentence := '';
        next := '';
        ExtractStartOfSentence(outputString, sentence, next);
        AppendCleanOutput(result, sentence);
        outputString := next;
        isPossibleEndOfSentence := false;
      end
      else if code = CPCLower then
      begin
        isPossibleEndOfSentence := false;
      end;
    end;

    outputString := outputString + codePoint;

    // At a full stop (.) or other chars (?, !): if next character is a space,
    // and next-next (or further) character is a capital,
    // then the sentence is considered as complete.
    if IsPossibleSplit(codePoint) then
    begin
      isPossibleEndOfSentence := true;
    end;

    inc(currentPosition, len);
  end;

  // Add the last string, if any
  AppendCleanOutput(result, outputString);
end;

end.


// Language Bath
// Copyright (c) 2020, 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// The code unit splits one (usually big) string into multiple sentences.
// It uses combinations of splitters: [.!?] plus spaces and lowercase/uppercase.
// This long sentence: "This text will be split. The value of pi is 3.14 usually, but sometimes 3.1415. Can you call Dr.Jones? Or is he called Prof. Dr. Jones? He is probably from London. What a funny bloke it is! Actually he is called A.B.C. Jones."
// will be splitted into:
//   This text will be split.
//   The value of pi is 3.14 usually, but sometimes 3.1415.
//   Can you call Dr.Jones?
//   Or is he called Prof. Dr. Jones?
//   He is probably from London.
//   What a funny bloke it is!
//   Actually he is called A.B.C. Jones.
// The dot requires special handling and will not always be correct, for example not for
// "I've been to Omsk. It's a Russian city." - because we cannot distinguish "Prof." and "Omsk."
// This limit can be specified, it is default 4.

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
  TCodePointClassification = (CPCUpper, CPCLower, CPCNumber, CPCSpace, CPCHash, CPCOther);

// Classifies a code point (of one UTF character) as uppercase, lowercase, number or other
function Classify(const codePoint : string) : TCodePointClassification;
var lower, upper : string;
begin
  result := CPCOther;

  // Spaces are most common in calling this function, return immediately
  if codePoint = ' ' then
  begin
    result := CPCSpace;
    exit;
  end;
  if codePoint = '#' then
  begin
    result := CPCHash;
    exit;
  end;

  if (length(codePoint) = 1) and (codePoint[1] >= '0') and (codePoint[1] <= '9') then
  begin
    result := CPCNumber;
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
    or (not (Classify(codePoint) in [CPCSpace, CPCOther])) then
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
  index, codePointSize: Integer;
  previousCode, code : TCodePointClassification;
  codePoint: String;

  next, sentence, outputString : string;
  isHashed, isPossibleTitle, isPossibleEndOfSentence : boolean;
  indexOfPossibleTitle, indexOfLowercase : integer;

begin
  previousCode := CPCOther;
  outputString := '';
  isPossibleEndOfSentence := false;
  isPossibleTitle := false;
  isHashed := false;
  indexOfPossibleTitle := 0;
  indexOfLowercase := 0;

  result := TStringList.Create;

  index := 0;
  codePoint := '';
  currentPosition := PChar(sentenceCollection);
  endPosition := currentPosition + length(sentenceCollection);
  while currentPosition < endPosition do
  begin
    inc(index);
    codePointSize := UTF8CodepointSize(currentPosition);
    SetLength(codePoint, codePointSize);
    Move(currentPosition^, codePoint[1], codePointSize);

    code := Classify(codePoint);

    if code = CPCLower then
    begin
      indexOfLowercase := index;
    end;

    if ((code = CPCUpper) or (code = CPCHash))
    and (previousCode = CPCSpace)
    and (index - indexOfLowercase < 4)
    then
    begin
      isHashed := code = CPCHash;
      isPossibleTitle := true;
      indexOfPossibleTitle := index;
    end
    else if isPossibleTitle and ((code = CPCSpace) or (index - indexOfPossibleTitle > 4)) then
    begin
      // At a space it's not a title anymore. Also if it's too long ago, to avoid
      // placenames being recognized as titles.
      isPossibleTitle := false;
    end;

    if isPossibleEndOfSentence then
    begin
      if code in [CPCUpper, CPCHash] then
      begin
        // New sentence detected. Append the current one and start again
        previousCode := CPCOther;
        sentence := '';
        next := '';
        ExtractStartOfSentence(outputString, sentence, next);
        AppendCleanOutput(result, sentence);
        outputString := next;
        isPossibleEndOfSentence := false;
      end
      else if (code in [CPCLower, CPCNumber]) and not isHashed then
      begin
        isPossibleEndOfSentence := false;
      end;
    end;

    outputString := outputString + codePoint;

    // At a full stop (.) or other chars (?, !):
    // If previous character wasn't a capital (to avoid A.B.C etc),
    // and if next character is a space,
    // and next-next (or further) character is a capital,
    // then the sentence is considered as complete.
    if ((previousCode <> CPCUpper) or (index - indexOfLowercase > 4))
    and not isPossibleTitle
    and IsPossibleSplit(codePoint) then
    begin
      isPossibleEndOfSentence := true;
    end;

    previousCode := code;
    inc(currentPosition, codePointSize);
  end;

  // Add the last string, if any
  AppendCleanOutput(result, outputString);
end;

end.


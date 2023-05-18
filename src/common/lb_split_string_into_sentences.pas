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

type
  THonorificEntry = record
    honorific : string;
    name : string;
    count : integer;
  end;

  THonorificArray = array of THonorificEntry;

function GetHonorifics(const sentenceCollection : string) : THonorificArray;
function SplitStringIntoSentences(const sentenceCollection : string; const map : THonorificArray) : TStringList;
procedure AddToMap(var a : THonorificArray; const f : string; const s : string; count : integer = 1);

implementation

uses LazUtf8, lb_quicksort;

type
  TCodePointClassification = (CPCUnknown, CPCUpper, CPCLower, CPCNumber, CPCSpace, CPCHash, CPCOther);

  TCompareByCountDesc = class
    function Less(const a, b : THonorificEntry) : boolean;
  end;

function TCompareByCountDesc.Less(const a, b : THonorificEntry) : boolean;
begin
  if a.count = b.count then result := a.honorific < b.honorific else result := a.count > b.count;
end;

procedure AddToMap(var a : THonorificArray; const f : string; const s : string; count : integer = 1);
var n : integer;
begin
  n := length(a);
  SetLength(a, n + 1);
  a[n].honorific := f;
  a[n].name := s;
  a[n].count := count;
end;

function CombiIndex(const a : THonorificArray; const f : string; const s : string) : integer;
var i : integer;
begin
  result := -1;
  for i := low(a) to high(a) do
  begin
    if (a[i].honorific = f) and (a[i].name = s) then
    begin
      result := i;
      exit;
    end;
  end;
end;

procedure AddOrSet(var map : THonorificArray; const f : string; const s : string; count : integer = 1);
var ci : integer;
begin
  ci := CombiIndex(map, f, s);
  if ci >= 0 then
  begin
    inc(map[ci].count, count);
  end
  else
  begin
    AddToMap(map, f, s, count);
  end;
end;

function HasCombi(const a : THonorificArray; const f : string; const s : string) : boolean;
begin
  result := CombiIndex(a, f, s) >= 0;
end;


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
    //or (codePoint = '“')
    //or (codePoint = '”')
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

function PeekNextWord(currentPosition, endPosition : PChar) : string;
var
  codePointSize: Integer;
  codePoint: String;
  code : TCodePointClassification;
  isEnd : boolean;
begin
  result := '';
  codePoint := '';
  isEnd := false;
  repeat
    codePointSize := UTF8CodepointSize(currentPosition);
    SetLength(codePoint, codePointSize);
    Move(currentPosition^, codePoint[1], codePointSize);
    inc(currentPosition, codePointSize);
    code := Classify(codePoint);
    isEnd := not (code in [CPCUpper, CPCLower]);
    if not isEnd then result := result + codePoint;
  until (currentPosition >= endPosition) or isEnd;
end;

// https://en.wikipedia.org/wiki/Honorific
function GetHonorifics(const sentenceCollection : string) : THonorificArray;
var
  currentPosition, endPosition: PChar;
  codePointSize: Integer;
  code : TCodePointClassification;
  codePoint: String;

  theLastWord, theNextWord : string;
  lastWord : string;
  possibleSplit : boolean;
  i, ci : integer;
  possibleHonorifics : THonorificArray;
  map : THonorificArray;

begin
  map := [];
  result := [];
  possibleHonorifics := [];
  lastWord := '';
  possibleSplit := false;
  codePoint := '';
  currentPosition := PChar(sentenceCollection);
  endPosition := currentPosition + length(sentenceCollection);
  while currentPosition < endPosition do
  begin
    codePointSize := UTF8CodepointSize(currentPosition);
    SetLength(codePoint, codePointSize);
    Move(currentPosition^, codePoint[1], codePointSize);

    code := Classify(codePoint);

    if possibleSplit then
    begin
      theLastWord := LowerCase(lastWord);
      if (theLastWord <> '') and (length(theLastWord) <= 4) then
      begin
         theNextWord := PeekNextWord(currentPosition + codePointSize, endPosition);
         if length(theNextWord) >= 2 then
         begin
           AddOrSet(map, theLastWord, theNextWord);
         end;
      end;
    end;

    if code = CPCSpace then
    begin
      lastWord := '';
    end
    else
    begin
      if code in [CPCUpper, CPCLower] then lastWord := lastWord + codePoint;
      possibleSplit := IsPossibleSplit(codePoint);
    end;

    inc(currentPosition, codePointSize);
  end;

  // Aggregate the found entries
  for i := low(map) to high(map) do
  begin
    if map[i].count >= 2 then
    begin
      AddOrSet(possibleHonorifics, map[i].honorific, '*', map[i].count);
    end;
  end;

  // Sort them descending
  specialize CallQuickSort<THonorificEntry, TCompareByCountDesc>(possibleHonorifics);
  specialize CallQuickSort<THonorificEntry, TCompareByCountDesc>(map);

  for i := low(possibleHonorifics) to high(possibleHonorifics) do
  begin
    if possibleHonorifics[i].count >= 2 then
    begin
      //writeln(possibleHonorifics[i].honorific, ',*,', possibleHonorifics[i].count);
      AddOrSet(result, possibleHonorifics[i].honorific, possibleHonorifics[i].name, possibleHonorifics[i].count);
    end;
  end;
  for i := low(map) to high(map) do
  begin
    if map[i].count >= 2 then
    begin
      //writeln(map[i].honorific, ',', map[i].name, ',', map[i].count);
      AddOrSet(result, map[i].honorific, map[i].name, map[i].count);
    end;
  end;
end;


function SplitStringIntoSentences(const sentenceCollection : string; const map : THonorificArray) : TStringList;
const debug = false;
var
  currentPosition, endPosition: PChar;
  index, codePointSize: Integer;
  previousCode, code : TCodePointClassification;
  codePoint: String;

  lastWord, next, sentence, outputString : string;
  theLastWord, theNextWord : string;
  isHashed, isPossibleEndOfSentence : boolean;


begin

  if debug then writeln('split ', sentenceCollection);

  previousCode := CPCOther;
  outputString := '';
  lastWord := '';
  isPossibleEndOfSentence := false;
  isHashed := false;

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

    if ((code = CPCUpper) or (code = CPCHash))
    and (previousCode = CPCSpace)
    then
    begin
      isHashed := code = CPCHash;
    end;

    if isPossibleEndOfSentence then
    begin
      if code in [CPCUpper, CPCHash] then
      begin
        // New sentence detected. Append the current one and start again
        if debug then writeln('sentence: ', outputString);
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

    if isPossibleEndOfSentence then
    begin
      theLastWord := LowerCase(lastWord);
      theNextWord := PeekNextWord(currentPosition + codePointSize, endPosition);
      if debug then writeln('Combi word: "', theLastWord, '" "', theNextWord, '" ', length(map));
      if HasCombi(map, theLastWord, theNextWord) then
      begin
        if debug then writeln('last word: "', lastWord, '" "', PeekNextWord(currentPosition + codePointSize, endPosition), '" ', isPossibleEndOfSentence);
        isPossibleEndOfSentence := false;
      end;
    end;

    if code = CPCSpace then
    begin
      if debug then writeln('Verify word: "', lastWord, '" "', PeekNextWord(currentPosition + codePointSize, endPosition), '" ', isPossibleEndOfSentence);
      lastWord := '';
    end
    else
    begin
      if not IsPossibleSPlit(codePoint) then lastWord := lastWord + codePoint;
    end;

    // At a full stop (.) or other chars (?, !):
    // If previous character wasn't a capital (to avoid A.B.C etc),
    // and if next character is a space,
    // and next-next (or further) character is a capital,
    // then the sentence is considered as complete.
    if (previousCode <> CPCUpper) and IsPossibleSplit(codePoint) then
    begin
      isPossibleEndOfSentence := true;
    end;

    previousCode := code;
    inc(currentPosition, codePointSize);
  end;

  // Add the last string, if any
  AppendCleanOutput(result, outputString);
  if false and debug then
  begin
    writeln('Last: ', outputString);
    for index := 0 to result.Count - 1 do
    begin
      writeln('Result: "', result[index], '"');
    end;
  end;
end;

end.


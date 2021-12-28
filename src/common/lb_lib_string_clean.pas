// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Functions to clean a string

unit lb_lib_string_clean;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function CleanText(const s: string): string;
function RemovePunctuations(const s: string): string;
function NormalizeWord(const s: string): string;


implementation

uses LazUtf8;

function RemovePunctuations(const s: string): string;
const
  punctuations: array of string = ('.', ';', ':', ',', '!', '?', '»', '«',
    '#', '"', '''', '|', '„', '“',
    '(', ')', '[', ']');
var
  i: integer;
begin
  Result := s;
  for i := low(punctuations) to high(punctuations) do
  begin
    Result := UTF8StringReplace(Result, punctuations[i], '', [rfReplaceAll]);
  end;
  result := UTF8Trim(result);
end;

function NormalizeWord(const s: string): string;
const
  punctuations: array of string = ('—', '–', '-', '~');
var
  i: integer;
begin
  Result := UTF8LowerString(s);
  for i := low(punctuations) to high(punctuations) do
  begin
    Result := UTF8StringReplace(Result, punctuations[i], '', [rfReplaceAll]);
  end;
end;


// Preprocessor functionality for splitting a text into strings
function CleanText(const s: string): string;
var
  currentPosition, endPosition: PChar;
  i, codePointSize: integer;
  codePoint: string;
  queue: array of string;
  skip: boolean;
begin
  // This code was first implemented with 7 UTF8StringReplace statements
  // but that takes several minutes on huge strings (e.g. 15000 sentences)
  // Therefore it is rewritten below in a more specific way, which runs in 2 seconds instead.
  queue := [];
  Result := '';
  currentPosition := PChar(s);
  endPosition := currentPosition + length(s);
  while currentPosition < endPosition do
  begin
    codePointSize := UTF8CodepointSize(currentPosition);
    codePoint := '';
    SetLength(codePoint, codePointSize);
    Move(currentPosition^, codePoint[1], codePointSize);

    // Make replacements of CR, tab, formfeed
    if (codePoint = #13) or (codePoint = #10) or (codePoint = #12) or
      (codePoint = #9) then
      codePoint := ' ';

    // Replace an ellipsis just with one dot
    if codePoint = '…' then
      codePoint := '.';

    // Add to the queue, but avoid adding "  " or ".."
    skip := ((codePoint = ' ') or (codePoint = '.')) and
      (length(queue) > 0) and (queue[length(queue) - 1] = codePoint);

    // Add current character to the queue
    if not skip then
    begin
      SetLength(queue, length(queue) + 1);
      queue[length(queue) - 1] := codePoint;
    end;

    // Add start of the queue to the result, keep 3 entries
    while length(queue) > 3 do
    begin
      Result := Result + queue[0];
      Delete(queue, 0, 1);
    end;

    // Check 3 entries
    // For dialogs (: « or : " or : - or similar),
    // force a sentence split (maybe move this to the split functionality)
    if (length(queue) = 3) and (queue[0] = ':') and (queue[1] = ' ') and
      ((queue[2] = '«') or (queue[2] = '-') or
      (queue[2] = '—') or (queue[2] = '–') or (queue[2] = '"')) then
      queue[0] := '.';

    Inc(currentPosition, codePointSize);
  end;

  for i := low(queue) to high(queue) do
  begin
    Result := Result + queue[i];
  end;
end;


end.


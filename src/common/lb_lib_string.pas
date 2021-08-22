unit lb_lib_string;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lb_types;

function HasEquivalents(const s : string) : boolean;
function ReplaceEquivalents(const s : string) : string;

function RussianReplaceAccents(const s : string) : string;
function RussianReplaceEquivalents(const s : string) : string;

function RemoveQuotes(const s : string) : string;
function RemovePunctuation(const s : string) : string;

// Removes accents / quotes
function DisplayString(const s : string) : string;

// Removes accents, quotes, punctuation, and makes it lower case
function BareString(const s : string) : string;
function Quoted(const s : string) : string;

function ArrayAsString(const a : array of integer) : string;

function CreateBagOfWords(const s : string) : TArrayOfString;
function InternationalSplitCharacters(const s : string) : TArrayOfString;

function LevenshteinDistance(const s, t : string): integer;

function IsCapital(const s : string) : boolean;

implementation

uses LazUTF8, Math;

const CSingleQuote : char = '''';

function IsCapital(const s : string) : boolean;
begin
  result := s = UTF8UpperString(s);
end;

function RussianReplaceAccents(const s : string) : string;
begin
  // Note that the letter-with-accents is a two Unicode character pair
  // (though sometimes shown as one)
  // Therefore walking through UnicodePoints here doesn't work, or would be more complex
  result := s;

  result := StringReplace(result, 'а́', 'а', [rfReplaceAll]);
  result := StringReplace(result, 'е́', 'е', [rfReplaceAll]);
  result := StringReplace(result, 'и́', 'и', [rfReplaceAll]);
  result := StringReplace(result, 'о́', 'о', [rfReplaceAll]);
  result := StringReplace(result, 'у́', 'у', [rfReplaceAll]);
  result := StringReplace(result, 'ы́', 'ы', [rfReplaceAll]);
  result := StringReplace(result, 'ю́', 'ю', [rfReplaceAll]);
  result := StringReplace(result, 'я́', 'я', [rfReplaceAll]);
  result := StringReplace(result, '''', '', [rfReplaceAll]);
end;

// Replaces Russian ё with е because in most books/texts it's omitted.
function RussianReplaceEquivalents(const s : string) : string;
begin
  result := StringReplace(s, 'ё', 'е', [rfReplaceAll]);
end;

function ReplaceEquivalents(const s : string) : string;
begin
  result := RussianReplaceEquivalents(s);
end;

function HasEquivalents(const s : string) : boolean;
begin
  result := UTF8Pos('ё', s) > 0;
end;

function Quoted(const s : string) : string;
begin
  result := CSingleQuote
    + StringReplace(s, CSingleQuote, CSingleQuote + CSingleQuote, [rfReplaceAll])
    + CSingleQuote;
end;

function RemoveQuotes(const s : string) : string;
begin
  result := StringReplace(s, CSingleQuote, '', [rfReplaceAll]);
end;

function RemovePunctuation(const s : string) : string;
const punctuations : array of string = (':', ';', ',', '!', '?', '»', '«',
                                        '(', ')', '[', ']', '{', '}',
                                        '—', '–', '"', '''');
var i : integer;
begin
  result := s;

  // Replace ellipses (...) or dots at the end of the sentence
  while pos('. ', result) > 0 do result := StringReplace(result, '. ', ' ', [rfReplaceAll]);
  while (length(result) > 0) and (result[length(result)] = '.') do delete(result, length(result), 1);

  // Note, don't replace dashes in e.g. 'кто-то'. So replace them with spaces, or at sentence start.
  result := trim(StringReplace(result, ' - ', ' ', [rfReplaceAll]));
  result := trim(StringReplace(result, ' -', ' ', [rfReplaceAll]));
  result := trim(StringReplace(result, '- ', ' ', [rfReplaceAll]));
  while (length(result) > 0) and (result[1] = '-') do delete(result, 1, 1);

  for i := low(punctuations) to high(punctuations) do
  begin
    result := StringReplace(result, punctuations[i], '', [rfReplaceAll]);
  end;

  // Change multiple spaces into single space
  while pos('  ', result) > 0 do result := StringReplace(result, '  ', ' ', [rfReplaceAll]);

  result := trim(result);
end;

function BareString(const s : string) : string;
begin
  result := RemovePunctuation(RemoveQuotes(
      RussianReplaceEquivalents(RussianReplaceAccents(UTF8LowerString(s)))));
end;

function DisplayString(const s : string) : string;
begin
  result := RemoveQuotes(RussianReplaceAccents(s));
end;

function MoveToResult(var s : string; var list : TStringList;
         const arr : array of string) : boolean;
var i : integer;
  item : string;
begin
  result := false;
  for i := low(arr) to high(arr) do
  begin
    item := arr[i];
    if copy(s, 1, length(item)) = item then
    begin
      list.append(item);
      delete(s, 1, length(item));
      result := true;
      exit;
    end;
  end;
end;


function CreateBagOfWords(const s : string) : TArrayOfString;

  procedure Add(var r : TArrayOfString; var part : string; var n : integer);
  begin
    if part <> '' then
    begin
      SetLength(r, n + 1);
      r[n] := part;
      part := '';
      inc(n);
    end;
  end;

var i, n : integer;
  part, ch : string;

begin
  result := [];
  part := '';
  n := 0;
  for i := 1 to Utf8Length(s) do
  begin
    ch := Utf8Copy(s, i, 1);
    if ch = ' ' then
    begin
      Add(result, part, n);
    end
    else
    begin
      part := part + ch;
    end;
  end;
  Add(result, part, n);
end;

// Based on : https://wiki.freepascal.org/UTF8_strings_and_characters
function InternationalSplitCharacters(const s : string) : TArrayOfString;
var
  CurP, EndP: PChar;
  n, Len: Integer;
  CodePoint: String;
begin
  result := [];
  n := 0;
  SetLength(result, n);
  CodePoint := '';
  CurP := PChar(S);
  EndP := CurP + length(S);
  while CurP < EndP do
  begin
    Len := UTF8CodepointSize(CurP);
    SetLength(CodePoint, Len);
    Move(CurP^, CodePoint[1], Len);

    SetLength(result, n + 1);
    result[n] := CodePoint;
    inc(n);

    inc(CurP, Len);
  end;
end;

// TODO: generic?
function ArrayAsString(const a : array of integer) : string;
var i : integer;
begin
  result := '';
  for i := low(a) to high(a) do
  begin
    if result <> '' then result := result + ', ';
    result := result + inttostr(a[i]);
  end;
  // SPECIFIC FOR SQL
  if result = '' then result := '-1';
end;

function LevenshteinDistanceOfBagOfWords(const s, t: array of string; out dbg : string): longint;
var
  d: array of array of integer;
  i, j, n, m: integer;
begin
  dbg := '';
  n := length(t);
  m := length(s);

  if n = 0 then
  begin
    result := m;
    exit;
  end;

  if m = 0 then
  begin
    result := n;
    exit;
  end;

  d := [[]];
  setlength(d, m + 1, n + 1);

  for i := 0 to m do d[i, 0] := i;
  for j := 0 to n do d[0, j] := j;
  for j := 1 to n do
  begin
    for i := 1 to m do
    begin
      if s[i - 1] = t[j - 1] then
      begin
        d[i, j] := d[i - 1, j -1]; // no operation
        dbg := dbg + s[i - 1];
      end
      else
      begin
        //dbg := dbg + ' <> ' + s[i - 1] + '/' + t[j - 1];
        d[i, j] := min(d[i - 1, j] + 1, // deletion
                   min(d[i, j - 1] + 1, // insertion
                       d[i - 1, j - 1] + 1)); // subsitution
      end;
    end;
  end;
  result := d[m, n];
end;

function LevenshteinDistance(const s, t : string): integer;
var sb, tb : array of string;
  dbg : string;
begin
  result := 0;
  sb := InternationalSplitCharacters(s);
  tb := InternationalSplitCharacters(t);

  if (length(sb) = 0) or (length(tb) = 0) then
  begin
    exit; // for now
  end;

  result := LevenshteinDistanceOfBagOfWords(sb, tb, dbg);

end;

end.



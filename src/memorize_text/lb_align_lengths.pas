unit lb_align_lengths;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function AlignLengths(const reference, entered: string): string;

implementation

uses LazUTF8, lb_lib_string;

function AlignLengthsAtBack(const reference, entered: string): string;
var minD, d, len, lenEntered, bestLen : integer;
  s : string;
begin
  bestLen := -1;
  minD := -1;
  lenEntered := Utf8Length(entered);
  len := Utf8Length(reference);
  while len > 0 do
  begin
    s := Utf8Copy(reference, 1, len);
    d := LevenshteinDistance(s, entered);
    if (minD = -1) or (d < minD) then
    begin
      minD := d;
      bestLen := len;
      result := s;
    end else if (len + 10 < bestLen) then
    begin
      // Too far back, it will not become better.
      exit;
    end;
    // TODO: move back until last space/tab/CR/LF
    if d > lenEntered div 2 then dec(len, 100)
    else if d > lenEntered div 10 then dec(len, 10)
    else dec(len);
  end;
end;

// Tries to find the best starting place where <entered> is part of <reference>
function AlignLengthsAtFront(const reference, entered: string): string;
const maxSub = 100;
var i, d, minD, lenRef, lenEntered : integer;
  s, bs, ch, cleaned : string;
begin
  minD := -1;
  cleaned := BareString(entered);
  lenEntered := Utf8Length(cleaned);
  if lenEntered > maxSub then
  begin
    // Too long - a part is sufficient, otherwise typo's might
    // influence the correct start point
    cleaned := Utf8Copy(cleaned, 1, maxSub);
    lenEntered := maxSub;
  end;
  lenRef := Utf8Length(reference);
  for i := 1 to lenRef - lenEntered do
  begin
    // The reference from this index to the end
    s := Utf8Copy(reference, i, lenRef);
    ch := Utf8Copy(s, 1, 1);
    // The reference from this index with the same length as the entered string,
    // plus a bit for some possibly missing characters in the entered text
    // (we might loop for this too)
    bs := Utf8Copy(BareString(s), 1, lenEntered + 10);
    // Calculate the distance over the two equally-length strings
    d := LevenshteinDistance(bs, cleaned);
    if (minD = -1)
    or (d < minD)
    or ((d = minD) and (IsPunctuation(ch) or IsSpacing(ch)))
    then
    begin
      minD := d;
      result := s;
    end
  end;
  // TODO: move back until last space/tab/CR/LF
end;

function AlignLengths(const reference, entered: string): string;
begin
  result := AlignLengthsAtBack(AlignLengthsAtFront(reference, entered), entered);
  result := Utf8Trim(result);
end;

end.


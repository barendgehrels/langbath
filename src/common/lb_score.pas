unit lb_score;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// Returns a value between 1.0 (perfect) and 0.0 (completely wrong)
// It's basically the Levenstein distance divided by the string length
// Some characters count less than others (for example russian soft sign)
function GetScore(const s, ref : string) : double;



implementation

uses LazUtf8, Math, lb_lib_string;

// The score depends on the length of the sentence, and the edit distance.
// If it's totally wrong (edit distance == length), then we might expect 0%.
// If it's completely right, then 100%. All others linearly in between
function Score(const s, ref : string) : double;
var len : integer;
begin
  // Get the minimum length (because if max is token, an empty string will result in 100%)
  result := 0;
  len := Math.Min(Utf8Length(s), Utf8Length(ref));
  if len > 0 then
  begin
    result := 1.0 - LevenshteinDistance(UTF8LowerCase(s), UTF8LowerCase(ref)) / len;
  end
end;

// Replace some characters to change the score a bit,
// it's less wrong (but still wrong) if these are not right.
function ReplaceEquivalents(const s : string) : string;
begin
  result := StringReplace(s, 'ё', 'е', [rfReplaceAll]);
  result := StringReplace(result, 'й', 'и', [rfReplaceAll]);
  result := StringReplace(result, 'ь', '', [rfReplaceAll]);
  result := StringReplace(result, 'ъ', '', [rfReplaceAll]);
end;

function GetScore(const s, ref : string) : double;
var s1, s2 : double;
begin
  s1 := Score(s, ref);
  s2 := Score(ReplaceEquivalents(s), ReplaceEquivalents(ref));
  result := (s1 + s2) / 2.0;
end;

end.

